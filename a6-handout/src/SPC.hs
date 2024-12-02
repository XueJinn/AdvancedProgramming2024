{-# LANGUAGE ScopedTypeVariables #-}
module SPC
  ( -- * SPC startup
    SPC,
    startSPC,

    -- * Job functions
    Job (..),
    JobId,
    JobStatus (..),
    JobDoneReason (..),
    jobAdd,
    jobStatus,
    jobWait,
    jobCancel,

    -- * Worker functions
    WorkerName,
    workerAdd,
    workerStop,
    -- Worker,
  )
where

import Control.Concurrent
  ( 
    forkIO,
    killThread,
    threadDelay,
    ThreadId,
  )
import Control.Exception (SomeException, catch)
import Control.Concurrent.Chan (newChan)
import Control.Monad ( ap, forever, liftM, void, forM_ )
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)
import Data.List (partition)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled, or the worker
    -- it was running on was stopped.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  | -- | A job with this ID is not known to this SPC instance.
    JobUnknown
  deriving (Eq, Ord, Show)

-- | A worker decides its own human-readable name. This is useful for
-- debugging.
type WorkerName = String

-- | Messages sent to workers. These are sent both by SPC and by
-- processes spawned by the workes.
data WorkerMsg -- TODO: add messages.
  = MsgJobExecute (JobId, Job)
  | MsgCancelJob (JobId, JobDoneReason)
  | MsgWorkerJobDone
  | MsgWorkerStop
-- Messages sent to SPC.
data SPCMsg
  = -- | Add the job, and reply with the job ID.
    MsgJobAdd Job (ReplyChan JobId)
  | -- | Cancel the given job.
    MsgJobCancel JobId
  | -- | Immediately reply the status of the job.
    MsgJobStatus JobId (ReplyChan JobStatus)
  | -- | Reply when the job is done.
    MsgJobWait JobId (ReplyChan JobDoneReason)
  | -- | Some time has passed.
    MsgTick
  | -- | Worker reports that a job is done.
    MsgJobDone JobId JobDoneReason
  | -- | Worker addition.
    MsgWorkerAdd WorkerName (ReplyChan (Either String Worker))
  | -- | Worker reports that it is stopping.
    MsgWorkerGone WorkerName


-- | A handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | A handle to a worker.
data Worker = Worker (Server WorkerMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobsRunning :: [(JobId, (Job, WorkerName, Seconds))],
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcJobCounter :: JobId,
    spcWorkers :: [(WorkerName, Worker)],
    spcWaiting :: [(JobId, ReplyChan JobDoneReason)],
    spcJobDoneChan :: Chan (JobId, JobDoneReason),
    spcWorkerGoneChan :: Chan WorkerName
  }

-- | The monad in which the main SPC thread runs. This is a state
-- monad with support for IO.
newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'

-- | Retrieve the state.
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

-- | Modify the state.
modify :: (SPCState -> SPCState) -> SPCM ()
modify f = do
  state <- get
  put $ f state

-- | Lift an 'IO' action into 'SPCM'.
io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

-- | Run the SPCM monad.
runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

-- | The monad in which the worker thread runs.
newtype WorkerM a = WorkerM (WorkerState -> IO (a, WorkerState))

instance Functor WorkerM where
  fmap = liftM

instance Applicative WorkerM where
  pure x = WorkerM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad WorkerM where
  WorkerM m >>= f = WorkerM $ \state -> do
    (x, state') <- m state
    let WorkerM f' = f x
    f' state'

-- | The worker's state.
data WorkerState = WorkerState
  { currentJob :: Maybe (JobId, ThreadId)
  }

-- | Retrieve the state.
getWorkerState :: WorkerM WorkerState
getWorkerState = WorkerM $ \state -> pure (state, state)

-- | Overwrite the state.
putWorkerState :: WorkerState -> WorkerM ()
putWorkerState state = WorkerM $ \_ -> pure ((), state)

-- -- | Modify the state.
-- modifyWorkerState :: (WorkerState -> WorkerState) -> WorkerM ()
-- modifyWorkerState f = do
--   state <- getWorkerState
--   putWorkerState $ f state

-- | Lift an 'IO' action into 'WorkerM'.
workerIO :: IO a -> WorkerM a
workerIO m = WorkerM $ \state -> do
  x <- m
  pure (x, state)

-- | Run the WorkerM monad.
runWorkerM :: WorkerState -> WorkerM a -> IO a
runWorkerM state (WorkerM f) = fst <$> f state

schedule :: SPCM ()
schedule = do
  state <- get
  let idleWorkers = filter (\(name, _) -> not $ any (\(_, (_, wname, _)) -> wname == name) (spcJobsRunning state)) (spcWorkers state)
  case idleWorkers of
    [] -> pure ()
    _ : _ -> forM_ idleWorkers $ \(name, worker) -> (do
                                                      flag <- workerExists name
                                                      if flag
                                                        then workerIsIdle name worker
                                                        else pure ())

jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobid reason = do
  state <- get
  case lookup jobid $ spcJobsDone state of
    Just _ ->
      -- We already know this job is done.
      pure ()
    Nothing -> do
      let (waiting_for_job, not_waiting_for_job) =
            partition ((== jobid) . fst) (spcWaiting state)
      forM_ waiting_for_job $ \(_, rsvp) ->
        io $ reply rsvp reason
      modify $ \s -> s { spcJobsDone = (jobid, reason) : spcJobsDone s,
                         spcJobsPending = removeAssoc jobid $ spcJobsPending s,
                         spcJobsRunning = removeAssoc jobid $ spcJobsRunning s,
                         spcWaiting = not_waiting_for_job
                       }

workerIsIdle :: WorkerName -> Worker -> SPCM ()
workerIsIdle name worker = do
  state <- get
  case spcJobsPending state of
    [] -> pure ()
    (jobid, job) : _ -> do
      let Worker workerServer = worker
      now <- io getSeconds
      modify $ \s -> s { spcJobsPending = tail (spcJobsPending s),
                         spcJobsRunning = (jobid, (job, name, now + fromIntegral (jobMaxSeconds job))) : spcJobsRunning s }
      -- Check if the worker is still running
      flag <- workerExists name
      if flag then io $ sendTo workerServer $ MsgJobExecute (jobid, job)
      else -- Worker is gone, reset the job to pending
        modify $ \s -> s { spcJobsPending = (jobid, job) : spcJobsPending s,
                           spcJobsRunning = removeAssoc jobid $ spcJobsRunning s }

workerIsGone :: WorkerName -> SPCM ()
workerIsGone wname = do
  modify $ \s -> s { spcWorkers = filter (\(wname', _) -> wname' /= wname) (spcWorkers s) }


checkTimeouts :: SPCM ()
checkTimeouts = do
  state <- get
  now <- io getSeconds
  let (timeOutJobs, inTimeJobs) = partition (\(_, (_, _, deadline)) -> now >= deadline) (spcJobsRunning state)
  forM_ timeOutJobs $ \(jobid, (_, wname, _)) -> do
    case lookup wname $ spcWorkers state of
      Just worker -> do
        let Worker workerServer = worker
        io $ sendTo workerServer $ MsgCancelJob (jobid, DoneTimeout)
      Nothing -> pure ()
  modify $ \s -> s { spcJobsRunning = inTimeJobs }


workerExists :: WorkerName -> SPCM Bool
workerExists name = do
  state <- get
  pure $ name `elem` map fst (spcWorkers state)

-- Check if spcJobsRunning has any job running on a worker that is not in spcWorkers
checkIfJobIsRunningOnDeadWorkers :: SPCM ()
checkIfJobIsRunningOnDeadWorkers = do
  state <- get
  runningJobs <- pure $ spcJobsRunning state
  forM_ runningJobs $ \(jobid, (job, wname, _)) -> do
    case lookup wname $ spcWorkers state of
      Just _ -> return ()
      Nothing -> do
        modify $ \s -> s { spcJobsPending = (jobid, job) : spcJobsPending s,
                           spcJobsRunning = removeAssoc jobid $ spcJobsRunning s }
      

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts
  checkIfJobIsRunningOnDeadWorkers
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending =
              (spcJobCounter state, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ reply rsvp $ JobId jobid
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $ reply rsvp $ case ( lookup jobid $ spcJobsPending state,
                               lookup jobid $ spcJobsRunning state,
                               lookup jobid $ spcJobsDone state
                             ) of
        (Just _, _, _) -> JobPending
        (_, Just _, _) -> JobRunning
        (_, _, Just r) -> JobDone r
        _ -> JobUnknown
    MsgWorkerAdd name rsvp -> do
      state <- get
      if name `elem` map fst (spcWorkers state)
        then io $ reply rsvp $ Left "Worker already exists"
        else do
          let jobDoneChan = spcJobDoneChan state
          let workerGoneChan = spcWorkerGoneChan state
          worker <- io $ spawn $ \worker_c -> workerHandleMsg name jobDoneChan workerGoneChan worker_c
          let workerHandle = Worker worker
          modify $ \s -> s { spcWorkers = (name, workerHandle) : spcWorkers s }
          io $ reply rsvp $ Right workerHandle
    MsgJobWait jobid rsvp -> do
      state <- get
      case lookup jobid $ spcJobsDone state of
        Just reason -> do
          io $ reply rsvp reason
        Nothing ->
          modify $ \s -> s { spcWaiting = (jobid, rsvp) : spcWaiting s }
    MsgJobCancel cancel_jobid -> do
      state <- get
      case lookup cancel_jobid $ spcJobsRunning state of
        Just (_, wname, _) -> do
          case lookup wname $ spcWorkers state of
            Just worker -> do
              let Worker workerServer = worker
              io $ sendTo workerServer $ MsgCancelJob (cancel_jobid, DoneCancelled)
            Nothing -> pure ()
        _ -> pure ()
    MsgJobDone jobid reason -> do
      jobDone jobid reason
    MsgTick ->
      pure ()
    MsgWorkerGone wname -> do
      workerIsGone wname


startSPC :: IO SPC
startSPC = do
  jobDoneChan <- newChan
  workerGoneChan <- newChan
  let initial_state =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = [],
            spcJobsRunning = [],
            spcJobsDone = [],
            spcWorkers = [],
            spcWaiting = [],
            spcJobDoneChan = jobDoneChan,
            spcWorkerGoneChan = workerGoneChan
          }
  spcServer <- spawn $ \c -> runSPCM initial_state $ forever $ handleMsg c
  void $ spawn $ timer spcServer
  -- Start a thread to listen for job done messages
  void $ forkIO $ jobDoneListener spcServer jobDoneChan
  void $ forkIO $ workerListener workerGoneChan spcServer
  pure $ SPC spcServer
  where
    timer spcServer _ = forever $ do
      threadDelay 1000000 -- 1 second
      sendTo spcServer MsgTick
    jobDoneListener spcServer jobDoneChan = forever $ do
      (jobid, reason) <- receive jobDoneChan
      sendTo spcServer $ MsgJobDone jobid reason
    workerListener workerGoneChan spcServer = forever $ do
      wname <- receive workerGoneChan
      sendTo spcServer $ MsgWorkerGone wname

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Asynchronously query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

-- | Synchronously block until job is done and return the reason.
jobWait :: SPC -> JobId -> IO JobDoneReason
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid

-- | Add a new worker with this name. Fails with 'Left' if a worker
-- with that name already exists.
workerAdd :: SPC -> WorkerName -> IO (Either String Worker)
workerAdd (SPC c) name = requestReply c $ MsgWorkerAdd name

workerHandleMsg :: WorkerName -> Chan (JobId, JobDoneReason) -> Chan WorkerName -> Chan WorkerMsg -> IO ()
workerHandleMsg wname jobDoneChan workerGoneChan c = do
  let initialWorkerState = WorkerState { currentJob = Nothing }
  runWorkerM initialWorkerState $ workerLoop wname jobDoneChan workerGoneChan c

workerLoop :: WorkerName -> Chan (JobId, JobDoneReason) -> Chan WorkerName -> Chan WorkerMsg -> WorkerM ()
workerLoop wname jobDoneChan workerGoneChan c = do
  continue <- workerProcessMsg jobDoneChan c
  if continue
    then workerLoop wname jobDoneChan workerGoneChan c
    else do
      -- Notify SPC that the worker has stopped
      workerIO $ send workerGoneChan wname
      return ()

workerProcessMsg :: Chan (JobId, JobDoneReason) -> Chan WorkerMsg -> WorkerM Bool
workerProcessMsg jobDoneChan c = do
  msg <- workerIO $ receive c
  case msg of
    MsgJobExecute (jobid, job) -> do
      state <- getWorkerState
      case currentJob state of
        Nothing -> do
          tid <- workerIO $ forkIO $ do
            do
              jobAction job
              send jobDoneChan (jobid, Done)
              `catch` (\(_ :: SomeException) -> send jobDoneChan (jobid, DoneCrashed))
            send c MsgWorkerJobDone
          putWorkerState state { currentJob = Just (jobid, tid) }
        Just _ -> do
          workerIO $ putStrLn "Worker is busy, cannot accept new job"
      return True
    MsgCancelJob (cancelJobId, reason) -> do
      state <- getWorkerState
      case currentJob state of
        Just (jobid, tid) | jobid == cancelJobId -> do
          workerIO $ killThread tid
          putWorkerState state { currentJob = Nothing }
          workerIO $ send jobDoneChan (jobid, reason)
        _ -> pure ()
      return True
    MsgWorkerJobDone -> do
      putWorkerState $ WorkerState { currentJob = Nothing }
      return True
    MsgWorkerStop -> do
      state <- getWorkerState
      case currentJob state of
        Just (jobid, tid) -> do
          workerIO $ killThread tid
          -- do not modify spc state in worker, let spc handle it
          putWorkerState state { currentJob = Nothing }
          workerIO $ send jobDoneChan (jobid, DoneCancelled)
          -- workerIO $ requestReply c $ MsgJobDone jobid DoneCancelled
        Nothing -> return ()
      return False

-- | Shut down a running worker. No effect if the worker is already
-- terminated.
workerStop :: Worker -> IO ()
workerStop (Worker workerServer) = sendTo workerServer MsgWorkerStop