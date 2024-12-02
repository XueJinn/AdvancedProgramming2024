module APL.InterpIO (runEvalIO) where

import APL.Monad
import APL.Util
import System.Directory (removeFile)
import System.IO (hFlush, readFile', stdout)

-- Converts a string into a value. Only 'ValInt's and 'ValBool' are supported.
readVal :: String -> Maybe Val
readVal = unserialize

-- 'prompt s' prints 's' to the console and then reads a line from stdin.
prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

-- 'writeDB dbFile s' writes the 'State' 's' to the file 'db'.
writeDB :: FilePath -> State -> IO ()
writeDB db s =
  writeFile db $ serialize s

-- 'readDB db' reads the database stored in 'db'.
readDB :: FilePath -> IO (Either Error State)
readDB db = do
  ms <- readFile' db
  case unserialize ms of
    Just s -> pure $ pure s
    Nothing -> pure $ Left "Invalid DB."

-- 'copyDB db1 db2' copies 'db1' to 'db2'.
copyDB :: FilePath -> FilePath -> IO ()
copyDB db db' = do
  s <- readFile' db
  writeFile db' s

-- Removes all key-value pairs from the database file.
clearDB :: IO ()
clearDB = writeFile dbFile ""

-- The name of the database file.
dbFile :: FilePath
dbFile = "db.txt"

-- Creates a fresh temporary database, passes it to a function returning an
-- IO-computation, executes the computation, deletes the temporary database, and
-- finally returns the result of the computation. The temporary database file is
-- guaranteed fresh and won't have a name conflict with any other files.
withTempDB :: (FilePath -> IO a) -> IO a
withTempDB m = do
  tempDB <- newTempDB -- Create a new temp database file.
  res <- m tempDB -- Run the computation with the new file.
  removeFile tempDB -- Delete the temp database file.
  pure res -- Return the result of the computation.

runEvalIO :: EvalM a -> IO (Either Error a)
runEvalIO evalm = do
  clearDB
  runEvalIO' envEmpty dbFile evalm
  where
    runEvalIO' :: Env -> FilePath -> EvalM a -> IO (Either Error a)
    runEvalIO' _ _ (Pure x) = pure $ pure x
    runEvalIO' r db (Free (ReadOp k)) = runEvalIO' r db $ k r
    runEvalIO' r db (Free (StateGetOp k)) = do
      s <- readDB db
      case s of
        Left e -> pure $ Left e
        Right state -> runEvalIO' r db $ k state
    runEvalIO' r db (Free (StatePutOp s m)) = do
      writeDB db s
      runEvalIO' r db m
    runEvalIO' r db (Free (PrintOp p m)) = do
      putStrLn p
      runEvalIO' r db m
   
    runEvalIO' _ _ (Free (ErrorOp e)) = pure $ Left e
    runEvalIO' r db (Free (TryCatchOp m1 m2)) = do
      res <- runEvalIO' r db m1
      case res of
        Left _ -> runEvalIO' r db m2
        Right result -> pure $ Right result
    runEvalIO' r db (Free (KvGetOp k kf)) = do
      s <- readDB db
      case s of
        Left e -> pure $ Left e
        -- what if the db is empty?
        Right state -> case lookup k state of
          Just v -> runEvalIO' r db $ kf v
          -- if key not found, inquire the user for a value and put it in the db
          Nothing -> do 
            v <- prompt $ "Invalid key: " ++ show k ++ ". Enter a replacement: "
            let v' = readVal v
            case v' of
              Just v'' -> do
                let dbState' = (k, v'') : state
                writeDB db dbState'
                runEvalIO' r db $ kf v''
              Nothing -> pure $ Left $ "Invalid value input: " ++ v


    runEvalIO' r db (Free (KvPutOp k v m)) = do
      dbState <- readDB db
      case dbState of
        Left _ -> do writeDB db [(k,v)]; runEvalIO' r db m
        Right s -> do
          let dbState' = update k v s
          writeDB db dbState'
          runEvalIO' r db m
          where 
            update :: Val -> Val -> State -> State
            update key value state = case lookup key state of
              Just _ -> (key, value) : filter (\(k', _) -> k' /= key) state
              Nothing -> (key, value) : state
    runEvalIO' r db (Free (TransactionOp em m)) = do
      withTempDB $ \tempDB -> do
        -- copy db to tempDB
        copyDB db tempDB
        res <- runEvalIO' r tempDB em
        case res of
          Left _ -> do
            runEvalIO' r db m
          Right _ -> do
            copyDB tempDB db
            runEvalIO' r db m

