-- | Simplified variant of SPC.
--
-- Always starts a new worker thread when a job is added (so nothing
-- is ever "pending"), removes support for timeouts, and adds a
-- function 'jobWaitAny' that allows us to wait for several jobs
-- concurrently.
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
    jobWaitAny,
    jobCancel,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM)
import Data.List (partition)
import GenServer

-- First some general utility functions.

-- Then the definition of the glorious SPC.

-- | A job that is to be enqueued in the glorious SPC.
newtype Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO ()
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
  | -- | The job was explicitly cancelled.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobsRunning
  | -- | A job with this ID is not known to this SPC instance.
    JobUnknown
  deriving (Eq, Ord, Show)

-- Messages sent to SPC.
data SPCMsg
  = -- | Add the job, and reply with the job ID.
    MsgJobAdd Job (ReplyChan JobId)
  | -- | Cancel the given job.
    MsgJobCancel JobId
  | -- | Immediately reply the status of the job.
    MsgJobStatus JobId (ReplyChan JobStatus)
  | -- | Reply when the job is done.
    MsgJobWait [JobId] (ReplyChan (JobId, JobDoneReason))
  | -- | Job has finished.
    MsgJobDone JobId
  | -- | Job crashed.
    MsgJobCrashed JobId

-- | A Handle to the SPC instance.
newtype SPC = SPC (Server SPCMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcChan :: Chan SPCMsg,
    spcJobsRunning :: [(JobId, ThreadId)],
    spcJobsDone :: [(JobId, JobDoneReason)],
    -- | These are waiting for this job to terminate.
    spcWaiting :: [([JobId], ReplyChan (JobId, JobDoneReason))],
    spcJobCounter :: Int
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

-- | Lift an 'IO' action into 'SPCM'.
io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

-- | Run the SPCM monad.
runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobid reason = do
  state <- get
  case lookup jobid $ spcJobsDone state of
    Just _ ->
      -- We already know this job is done.
      pure ()
    Nothing -> do
      let (waiting_for_job, not_waiting_for_job) =
            partition (elem jobid . fst) (spcWaiting state)
      forM_ waiting_for_job $ \(_, rsvp) ->
        io $ reply rsvp (jobid, reason)
      put $
        state
          { spcWaiting = not_waiting_for_job,
            spcJobsDone = (jobid, reason) : spcJobsDone state
          }

addJob :: Job -> SPCM JobId
addJob job = do
  state <- get
  let jobid = JobId $ spcJobCounter state
  tid <- io $ forkIO $ do
    let doJob = do
          jobAction job
          send (spcChan state) $ MsgJobDone jobid
        onException :: SomeException -> IO ()
        onException _ =
          send (spcChan state) $ MsgJobCrashed jobid
    doJob `catch` onException
  put
    state
      { spcJobCounter = succ $ spcJobCounter state,
        spcJobsRunning = (jobid, tid) : spcJobsRunning state
      }
  pure jobid

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      jobid <- addJob job
      io $ reply rsvp jobid
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $ reply rsvp $ case ( lookup jobid $ spcJobsRunning state,
                               lookup jobid $ spcJobsDone state
                             ) of
        (Just _, _) -> JobsRunning
        (_, Just r) -> JobDone r
        _ -> JobUnknown
    MsgJobWait jobids rsvp -> do
      state <- get
      case filter (flip elem jobids . fst) $ spcJobsDone state of
        (jobid, reason) : _ -> do
          io $ reply rsvp (jobid, reason)
        _ ->
          put $ state {spcWaiting = (jobids, rsvp) : spcWaiting state}
    MsgJobDone jobid -> do
      state <- get
      case lookup jobid $ spcJobsRunning state of
        Just _ ->
          jobDone jobid Done
        _ -> pure ()
    MsgJobCancel jobid -> do
      state <- get
      case lookup jobid $ spcJobsRunning state of
        Just tid -> do
          io $ killThread tid
          jobDone jobid DoneCancelled
        _ -> pure ()
    MsgJobCrashed jobid -> do
      state <- get
      case lookup jobid $ spcJobsRunning state of
        Just tid -> do
          io $ killThread tid
          jobDone jobid DoneCrashed
        _ -> pure ()

startSPC :: IO SPC
startSPC = do
  let initial_state c =
        SPCState
          { spcJobCounter = 0,
            spcJobsRunning = [],
            spcJobsDone = [],
            spcWaiting = [],
            spcChan = c
          }
  server <- spawn $ \c -> runSPCM (initial_state c) $ forever $ handleMsg c
  pure $ SPC server

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

-- | Synchronously block until one of the given jobs is done and
-- return the reason.
jobWaitAny :: SPC -> [JobId] -> IO (JobId, JobDoneReason)
jobWaitAny (SPC c) jobids =
  requestReply c $ MsgJobWait jobids

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid
