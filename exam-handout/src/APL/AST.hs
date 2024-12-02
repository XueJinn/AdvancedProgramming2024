module APL.AST
  ( VName,
    Exp (..),
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | Lambda VName Exp
  | Apply Exp Exp
  | -- | Although the AST can represent 1-element tuples, that is
    -- still not allowed according to the surface grammar.
    Tuple [Exp]
  | Project Exp Integer
  | ForLoop (VName, Exp) (VName, Exp) Exp
  | WhileLoop (VName, Exp) Exp Exp
  | BothOf Exp Exp
  | OneOf Exp Exp
  | KvPut Exp Exp
  | KvGet Exp
  deriving (Eq, Ord, Show)
