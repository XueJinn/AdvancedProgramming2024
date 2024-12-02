module APL.InterpSim (runEval) where

import APL.Monad

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

-- | Continue execution of the provided computation as far as
-- possible, but executing at most one 'StepOp' effect. Any nested
-- computations (in 'BothOp' and 'OneOfOp') must also be stepped
-- similarly. If the computation is stuck on a 'KvGetOp' for which the
-- key is not in the state, then the computation is merely returned
-- unchanged.
--
-- Evaluation of 'BothOp':
--
-- * If either of the nested computations are 'Free (ErrorOp ...)',
--   then propagate that error.
--
-- * If both are 'Pure', then return a pair of the results.
--
-- * Otherwise evaluate both one step.
--
-- Evaluation of 'OneOfOp':
--
-- * If both of the nested computations are 'Free (ErrorOp ...)', then
--   propagate one of the errors.
--
-- * If one is 'Pure', then return that result.
--
-- * Otherwise evaluate both one step.
step :: Env -> State -> EvalM a -> (EvalM a, State)
step _ s (Pure x) = (pure x, s)
step r s (Free (ReadOp k)) = step r s $ k r
step _ s (Free (ErrorOp e)) = (Free $ ErrorOp e, s)
step _ s (Free (StepOp m)) = (m, s)
step r s (Free (KvGetOp key k)) = 
  case lookup key s of
    Nothing -> (Free $ KvGetOp key k, s)
    Just val -> step r s $ k val
step r s (Free (KvPutOp key val m)) = 
  let s' = (key, val) : filter ((/= key) . fst) s
   in step r s' m
step r s (Free (BothOfOp m1 m2 k)) = 
    let (m1', s1) = step r s m1
        (m2', s2) = step r s1 m2
    in case (m1', m2') of
      (Free (ErrorOp e1), _) -> (Free $ ErrorOp e1, s2)
      (_, Free (ErrorOp e2)) -> (Free $ ErrorOp e2, s2)
      (Pure v1, Pure v2) -> step r s2 (k (ValTuple [v1, v2]))
      _ -> (Free $ BothOfOp m1' m2' k, s2)
step r s (Free (OneOfOp m1 m2 k)) = 
  let (m1', s1) = step r s m1
  in case m1' of
    Pure v1 -> step r s1 (k v1)
    _ -> let (m2', s2) = step r s1 m2
         in case m2' of
              Pure v2 -> step r s2 (k v2)
              _ -> (Free $ OneOfOp m1' m2' k, s2)
runEval :: EvalM a -> Either Error a
runEval = runEval' envEmpty stateInitial
  where 
    runEval' r s m' = case step r s m' of
      (Pure x, _) -> Right x
      (Free (ErrorOp e), _) -> Left e
      (m'', s') -> runEval' r s' m''
