module APL.InterpPure (runEval) where

import APL.Monad

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

runEval :: EvalM a -> Either Error a
runEval = fmap fst $ runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> (Either Error a, State)
    runEval' _ s (Pure x) = (pure x, s)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (KvGetOp key k)) =
      case lookup key s of
        Nothing -> (Left $ "Invalid key: " ++ show key, s)
        Just val -> runEval' r s $ k val
    runEval' r s (Free (KvPutOp key val m)) =
      let s' = (key, val) : filter ((/= key) . fst) s
       in runEval' r s' m
    runEval' _ s (Free (ErrorOp e)) = (Left e, s)
    runEval' r s (Free (StepOp m)) = runEval' r s m
    runEval' r s (Free (BothOfOp m1 m2 m)) = 
      case runEval' r s m1 of
        (Left e1, s') -> (Left e1, s')
        (Right v1, s') -> case runEval' r s' m2 of
          (Left e2, s'') -> (Left e2, s'')
          (Right v2, s'') -> runEval' r s'' (m (ValTuple [v1, v2]))
    runEval' r s (Free (OneOfOp m1 m2 m)) = 
      case runEval' r s m1 of
        (Right v1, s') -> runEval' r s' (m v1)
        (Left _, s') -> case runEval' r s' m2 of
          (Right v2, s'') -> runEval' r s'' (m v2)
          (Left e2, s'') -> (Left e2, s'')