module APL.InterpConcurrent (runEval) where

import APL.Monad
import Data.IORef
import KVDB
import SPC

runEval :: EvalM a -> IO (Either Error a)
runEval m = do
    spc <- startSPC
    kvdb <- startKVDB
    interpConcurrent kvdb spc m

interpConcurrent :: KVDB Val Val -> SPC -> EvalM a -> IO (Either Error a)
interpConcurrent kvdb spc = interpConcurrent' envEmpty
  where 
    interpConcurrent' _ (Pure x) = pure $ Right x
    interpConcurrent' env (Free (ReadOp k)) = interpConcurrent' env (k env)
    interpConcurrent' _ (Free (ErrorOp e)) = pure $ Left e
    interpConcurrent' env (Free (StepOp m)) = interpConcurrent' env m
    interpConcurrent' env (Free (KvGetOp key k)) = do
      v <- kvGet kvdb key
      interpConcurrent' env (k v)
    interpConcurrent' env (Free (KvPutOp key val m)) = do
      kvPut kvdb key val
      interpConcurrent' env m
    interpConcurrent' env (Free (BothOfOp m1 m2 k)) = do
        ref1 <- newIORef (Left "Task 1 not done")
        ref2 <- newIORef (Left "Task 2 not done")
        jobid1 <- jobAdd spc $ Job $ do
            result1 <- interpConcurrent kvdb spc m1
            writeIORef ref1 result1
        jobid2 <- jobAdd spc $ Job $ do
            result2 <- interpConcurrent kvdb spc m2
            writeIORef ref2 result2
        (_, reason1) <- jobWaitAny spc [jobid1]
        (_, reason2) <- jobWaitAny spc [jobid2]
        case (reason1, reason2) of  
          (Done, Done) -> do
            result1 <- readIORef ref1
            result2 <- readIORef ref2
            case (result1, result2) of  
              (Right v1, Right v2) -> interpConcurrent' env (k (ValTuple [v1, v2]))
              (Left e1, _) -> pure $ Left e1
              (_, Left e2) -> pure $ Left e2
          _ -> pure $ Left "Not both tasks done"
    interpConcurrent' env (Free (OneOfOp m1 m2 k)) = do
        ref1 <- newIORef (Left "Task 1 not done")
        ref2 <- newIORef (Left "Task 2 not done")
        jobid1 <- jobAdd spc $ Job $ do
            result1 <- interpConcurrent kvdb spc m1
            writeIORef ref1 result1
        jobid2 <- jobAdd spc $ Job $ do
            result2 <- interpConcurrent kvdb spc m2
            writeIORef ref2 result2
        (finishedJobId, reason1) <- jobWaitAny spc [jobid1, jobid2]
        let (resultRef, runningJobId) = if finishedJobId == jobid1 
            then (ref1, jobid2) 
            else (ref2, jobid1)
        let runningRef = if finishedJobId == jobid1 then ref2 else ref1
        firstResult <- readIORef resultRef
        case (firstResult, reason1) of
            (Right v, Done) -> do
                jobCancel spc runningJobId
                interpConcurrent' env (k v)
            _ -> do
                (_, reason2) <- jobWaitAny spc [runningJobId]
                case reason2 of
                    Done -> do
                        secondResult <- readIORef runningRef
                        case secondResult of
                            Right v -> interpConcurrent' env (k v)
                            Left e -> pure $ Left e
                    _ -> pure $ Left "Both tasks not done"