module Language.Lambda.Types(
  Core(..),
  IsPrim(..),
) where

import Data.Text (Text)

-- | Core language
data Core p v
  = Prim p
  | Op ((PrimOp p) (Core p v))
  | Var v
  | App (Core p v) (Core p v)
  | Lam v (Core p v)
  | If (Core p v) (Core p v) (Core p v)
  | Fix (Core p v)
  | Let v (Core p v) (Core p v)


class (Functor (PrimOp p), Monad (EvalMonad p)) => IsPrim p where
  type PrimOp p :: * -> *
  type EvalMonad p :: * -> *
  fromPrimBool :: p -> Maybe Bool
  evalPrim :: (PrimOp p) p -> EvalMonad p (Either Text p)
