{-# LANGUAGE InstanceSigs #-}
module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]
-- type State = [String]
-- extend the State type to also contain a key-value mapping
type State = ([String], [(Val, Val)])

envEmpty :: Env
envEmpty = []

stateEmpty :: State
stateEmpty = ([], [])

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

-- newtype EvalM a = EvalM (Env -> Either Error a)
-- combined reader-and-state monad
newtype EvalM a = EvalM (Env -> State -> (State, Either Error a))

-- newtype EvalM' a = EvalM' (Env -> Either Error (State, a))



instance Functor EvalM where
  fmap :: (a -> b) -> EvalM a -> EvalM b
  fmap = liftM

instance Applicative EvalM where
  -- pure :: a -> EvalM a
  pure x = EvalM $ \_env state -> (state, Right x)
  (<*>) = ap

instance Monad EvalM where
  return = pure
  -- (>>=) :: EvalM a -> (a -> EvalM b) -> EvalM b
  EvalM x >>= f = EvalM $ \env state ->
    case x env state of
      (s, Left err) -> (s, Left err)
    --   ((s, d), Right x') -> 
    --     let EvalM m = f x'
    --     in let ((s', d'), result) = m env (s, d)
    --     in ((s <> s', d'), result)
      (s, Right x') -> case f x' of
        EvalM m -> m env s


-- runRS :: env -> s -> RS env s a -> (a, s)
-- runRS env state (RS f) = f env state

-- rsGet :: RS env s s
-- rsGet = RS $ \_env state -> (state, state)

-- rsPut :: s -> RS env s ()
-- rsPut state = RS $ \_env _ -> ((), state)

-- rsAsk :: RS env s env
-- rsAsk = RS $ \env state -> (env, state)

-- rsLocal :: (env -> env) -> RS env s env -> RS env s env
-- rsLocal f (RS g) = RS $ \env state -> g (f env) state

askEnv :: EvalM Env
askEnv = EvalM $ \env state -> (state, Right env)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env state -> m (f env) state

failure :: String -> EvalM a
failure s = EvalM $ \_env state -> (state, Left s)

-- askState :: EvalM State
-- askState = EvalM $ \_env state -> (state, Right state)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env state->
  case m1 env state of
    (_, Left _) -> case m2 env state of
      (s', Right x) -> (s', Right x)
      (s', Left err) -> (s', Left err)
    (s, Right x) -> (s, Right x)



runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) = case m envEmpty stateEmpty of
  ((s, _), Left err) -> (s, Left err)
  ((s, _), Right x) -> (s, Right x)

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y


evalPrint :: String -> EvalM ()
evalPrint str = EvalM $ \_env (s, d) -> ((s++[str], d), Right ())

evalKvGet :: Val -> EvalM Val
evalKvGet k = EvalM $ \_env (s, d) ->
  case lookup k d of
    Just v -> ((s, d), Right v)
    Nothing -> ((s, d), Left $ "Invalid key: " ++ show k)

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut k v = EvalM $ \_env (s, d) -> --((s, (k, v) : d), Right ())
  case lookup k d of
    Nothing -> ((s, (k, v) : d), Right ())
    Just _ -> ((s, (k, v) : filter (\(key, _) -> key /= k) d), Right ())

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ x ^ y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2
eval (Print str e) = do
  value <- eval e
  case value of
    ValInt x -> evalPrint (str ++ ": " ++ show x) >> return value
    ValBool x -> evalPrint (str ++ ": " ++ show x) >> return value
    ValFun _ _ _ -> evalPrint (str ++ ": #<fun>") >> return value

eval (KvPut k v) = do
    --If there is already an association for the value k, it is
  -- replaced by the new association.
  key <- eval k
  value <- eval v
  evalKvPut key value
  return value

eval (KvGet k) = do
  key <- eval k
  evalKvGet key
  


-- generate some sample data
-- runEval $ eval (KvPut (CstInt 1) (CstInt 2))
-- runEval $ eval $ Print "foo" $ KvPut (CstInt 1) (CstInt 2)
-- generate a sample which has a duplicate key
-- runEval $ eval $ Let "foo" (KvPut (CstInt 1) (CstInt 2)) (KvPut (CstInt 1) (CstInt 3))
-- runEval $ eval $ Let "result" (KvPut (CstInt 1) (CstInt 100))  (Let "result2" (KvPut (CstInt 1) (CstInt 200)) (KvGet (CstInt 1))) 
-- runEval $ eval $ Let "result1" (KvPut (CstInt 1) (CstInt 100))
--                    (Let "result2" (KvPut (CstInt 1) (CstInt 200))
--                    (Let "get1" (KvGet (CstInt 1))
--                    (KvGet (CstInt 1))))