module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Error = String

type Env = [VName]
envEmpty :: Env
envEmpty = []

envLookup :: VName -> Env -> Maybe VName
envLookup v env = case env of
    [] -> Nothing
    (x:xs) -> if v == x then Just v else envLookup v xs

envExtend :: VName -> Env -> Env
envExtend v env = v : env

newtype CheckM a = CheckM (Env -> Either Error a)

runCheckM :: CheckM a -> Maybe Error
runCheckM (CheckM m) = case m envEmpty of
    Left e -> Just e
    Right _ -> Nothing

instance Functor CheckM where
    fmap = liftM

instance Applicative CheckM where
    pure x = CheckM $ \_ -> Right x
    (<*>) = ap

instance Monad CheckM where
    return = pure
    (CheckM m) >>= f = CheckM $ \env -> case m env of
        Left e -> Left e
        Right x -> case f x of
            CheckM m' -> m' env

askEnv :: CheckM Env
askEnv = CheckM $ \env -> Right env

localEnv :: (Env -> Env) -> CheckM a -> CheckM a
localEnv f (CheckM m) = CheckM $ \env -> m (f env)

throwError :: Error -> CheckM a
throwError e = CheckM $ \_ -> Left e


check :: Exp -> CheckM ()
check (CstInt _) = return ()
check (CstBool _) = return ()
check (Add e1 e2) = do
    check e1
    check e2
check (Sub e1 e2) = do
    check e1
    check e2
check (Mul e1 e2) = do
    check e1
    check e2
check (Div e1 e2) = do
    check e1
    check e2
check (Pow e1 e2) = do
    check e1
    check e2
check (Eql e1 e2) = do
    check e1
    check e2
check (If e1 e2 e3) = do
    check e1
    check e2
    check e3
check (Var x) = do 
    env <- askEnv
    case envLookup x env of
        Just _ -> return ()
        Nothing -> throwError $ "Variable not in scope: " ++ x
check (Let x e1 e2) = do
    check e1
    localEnv (envExtend x) (check e2)
check (Lambda x e) = do
    localEnv (envExtend x) (check e)
check (Apply e1 e2) = do
    check e1
    check e2
check (TryCatch e1 e2) = do
    check e1
    check e2
check (Print _ e) = do
    check e
check (KvPut e1 e2) = do
    check e1
    check e2
check (KvGet e) = do
    check e

checkExp :: Exp -> Maybe Error
checkExp e = runCheckM (check e)
