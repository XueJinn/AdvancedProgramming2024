module APL.Monad
  ( envEmpty,
    envExtend,
    envLookup,
    askEnv,
    localEnv,
    failure,
    evalStep,
    evalKvGet,
    evalKvPut,
    evalBothOf,
    evalOneOf,
    EvalM,
    Val (..),
    EvalOp (..),
    Free (..),
    Error,
    Env,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  | -- | Although this value representation allows 1-element tuples,
    -- that is still not allowed according to the surface grammar.
    ValTuple [Val]
  deriving (Eq, Ord, Show)

type Error = String

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

data Free e a
  = Pure a
  | Free (e (Free e a))

instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free g) = Free $ fmap (fmap f) g

instance (Functor e) => Applicative (Free e) where
  pure = Pure
  (<*>) = ap

instance (Functor e) => Monad (Free e) where
  Pure x >>= f = f x
  Free g >>= f = Free $ h <$> g
    where
      h x = x >>= f

data EvalOp a
  = ReadOp (Env -> a)
  | ErrorOp Error
  | KvGetOp Val (Val -> a)
  | KvPutOp Val Val a
  | -- | A vacuous effect that has no influence on APL as such, but
    -- allows an interpretation function to take back control.
    StepOp a
  | -- | This effect corresponds to the '&&' operator. The
    -- continuation must be invoked with a pair ('ValTuple') of the
    -- results of the two 'EvalM Val' computations.
    BothOfOp (EvalM Val) (EvalM Val) (Val -> a)
  | -- | This effect corresponds to the '||' operator. The
    -- continuation must be invoked with the result of either of the
    -- 'EvalM Val' computations.
    OneOfOp (EvalM Val) (EvalM Val) (Val -> a)

instance Functor EvalOp where
  fmap f (ReadOp c) = ReadOp $ f . c
  fmap f (KvGetOp key k) = KvGetOp key $ f . k
  fmap f (KvPutOp key val m) = KvPutOp key val $ f m
  fmap _ (ErrorOp e) = ErrorOp e
  fmap f (StepOp c) = StepOp $ f c
  fmap f (BothOfOp x y c) = BothOfOp x y $ f . c
  fmap f (OneOfOp x y c) = OneOfOp x y $ f . c

type EvalM a = Free EvalOp a

askEnv :: EvalM Env
askEnv = Free $ ReadOp $ \env -> pure env

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv _ (Pure x) = Pure x
localEnv _ (Free (ErrorOp e)) = Free (ErrorOp e)
localEnv f (Free (ReadOp c)) = Free $ ReadOp $ localEnv f . c . f
localEnv f (Free (KvGetOp key c)) = Free $ KvGetOp key $ localEnv f . c
localEnv f (Free (KvPutOp key val c)) = Free $ KvPutOp key val $ localEnv f c
localEnv f (Free (StepOp c)) = Free $ StepOp $ localEnv f c
localEnv f (Free (BothOfOp x y c)) =
  Free $ BothOfOp (localEnv f x) (localEnv f y) $ localEnv f . c
localEnv f (Free (OneOfOp x y c)) =
  Free $ OneOfOp (localEnv f x) (localEnv f y) $ localEnv f . c

failure :: String -> EvalM a
failure = Free . ErrorOp

-- | Perform a StepOp effect, then perform the provided computation.
evalStep :: EvalM a -> EvalM a
evalStep m = Free $ StepOp m

evalKvGet :: Val -> EvalM Val
evalKvGet key = Free $ KvGetOp key $ \val -> pure val

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut key val = Free $ KvPutOp key val $ pure ()

-- | Produce a BothOfOp effect.
evalBothOf :: EvalM Val -> EvalM Val -> EvalM Val
evalBothOf a b = Free $ BothOfOp a b pure

-- | Produce a OneOfOp effect.
evalOneOf :: EvalM Val -> EvalM Val -> EvalM Val
evalOneOf a b = Free $ OneOfOp a b pure
