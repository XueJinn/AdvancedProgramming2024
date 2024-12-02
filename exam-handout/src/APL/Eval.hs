module APL.Eval
  ( eval,
  )
where

import APL.AST (Exp (..))
import APL.Monad

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
    _ -> failure "If: non-boolean conditional"
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
      localEnv (const $ envExtend var arg f_env) $ evalStep $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (Tuple es) =
  if length es == 1
    then failure "Illegal tuple"
  else do
    let evalOne [] acc = pure $ reverse acc
        evalOne (e:rest) acc = do
          v <- eval e
          evalOne rest (v:acc)
    ValTuple <$> evalOne es [] 
eval (Project e i) = do
  v <- eval e
  case v of
    ValTuple vs -> if i < 0 || i >= fromIntegral (length vs)
      then failure "Tuple index out of bounds"
      else pure $ vs !! fromIntegral i
    _ -> failure "Cannot project non-tuple"
eval (ForLoop (pvar, initp) (ivar, bound) body) = do
  initVal <- eval initp
  boundVal <- eval bound
  case boundVal of
    ValInt n -> 
      let loop i val | i >= n = pure val
                     | otherwise = do 
                        newVal <- localEnv (envExtend ivar (ValInt i) . envExtend pvar val) $ evalStep $ eval body
                        loop (i + 1) newVal
      in loop 0 initVal
    _ -> failure "Forloop: bound must be an integer"
eval (WhileLoop (pvar, initp) cond body) = do
  initVal <- eval initp
  let loop val = do 
        condVal <- localEnv (envExtend pvar val) $ eval cond
        case condVal of
          ValBool True -> do
            newVal <- localEnv (envExtend pvar val) $ evalStep $ eval body
            loop newVal
          ValBool False -> pure val
          _ -> failure "WhileLoop: conditional must be a boolean"
  loop initVal
eval (BothOf e1 e2) = evalBothOf (eval e1) (eval e2)
eval (OneOf e1 e2) = evalOneOf (eval e1) (eval e2)
eval (KvPut e1 e2) = do
  key <- eval e1
  value <- eval e2
  evalKvPut key value
  return value
eval (KvGet e) = do
  key <- eval e
  evalKvGet key