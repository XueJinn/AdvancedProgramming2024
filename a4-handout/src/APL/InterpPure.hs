module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)
    runEval' r s (Free (TryCatchOp m1 m2)) = case runEval' r s m1 of
      (_, Left _) -> runEval' r s m2
      (ps, res) -> (ps, res)
    runEval' r s (Free (KvGetOp k kf)) = case lookup k s of
      Just v -> runEval' r s $ kf v
      Nothing -> ([], Left $ "Invalid key: " ++ show k)
    runEval' r s (Free (KvPutOp k v m)) = runEval' r (update k v s) m where
      update :: Val -> Val -> State -> State
      update key value state = case lookup key state of
        Just _ -> (key, value) : filter (\(k', _) -> k' /= key) state
        Nothing -> (key, value) : state
    runEval' r s (Free (TransactionOp em m)) =
      let (ps, newState) = runEval' r s (em >> getState) in 
      -- note that res is the new state
      -- then get the result of the runEval' em
      case newState of
        -- Right newState' -> let (ps1, res_em) = runEval' r s em in
        --   case res_em of
        --     Left e -> let (ps2, res_m) = runEval' r s m in (ps1 ++ ps2, res_m)
        --     Right _ -> let (ps2, res_m) = runEval' r newState' m in (ps1 ++ ps2, res_m)
        -- Left e -> let (ps1, res_em) = runEval' r s em in
        --   case res_em of
        --     Left e -> let (ps2, res_m) = runEval' r s m in (ps1 ++ ps2, res_m)
        --     Right _ -> let (ps2, res_m) = runEval' r s m in (ps1 ++ ps2, res_m)
        Right newState' -> 
          -- this means that em succeeded, then the result of em should be discarded
          let (ps1, res_m) = runEval' r newState' m in (ps ++ ps1, res_m)
        Left _ -> 
          -- this means that em failed, then the result of em should be discarded and state should be rolled back
          let (ps1, res_m) = runEval' r s m in (ps ++ ps1, res_m)