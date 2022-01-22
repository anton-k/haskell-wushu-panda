module Language.Lambda.Eval(
  eval,
  Env(..),
) where

import Data.Map.Strict (Map)
import Language.Lambda.Types
import Data.Text (Text)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

data Val p v
  = PrimVal p
  | FunVal (Core p v -> Core p v)

-- | Map of predefined definitions
newtype Env p v = Env { getEnv :: (Map v (Core p v)) }

eval :: (IsPrim p, Show v, Ord v)
  => Env p v
  -> Core p v
  -> EvalMonad p (Either Text p)
eval env expr = undefined

eval' :: (IsPrim p, Show v, Ord v)
  => Env p v
  -> Core p v
  -> EvalMonad p (Either Text (Val p v))
eval' env expr = case expr of
  Prim p       -> evalPrim p
  Op op        -> evalOp op
  Var v        -> evalVar v
  App a b      -> evalApp a b
  Lam v body   -> evalLam v body
  If c t e     -> evalIf c t e
  Fix e        -> evalFix e
  Let v e body -> evalLet v e body
  where
    evalPrim p = pure $ Right $ PrimVal p

    evalOp e = undefined

    evalVar v = case M.lookup v (getEnv env) of
      Just e  -> rec e
      Nothing -> pure $ Left $ T.unwords ["Var", T.pack (show v) ,"not found"]

    evalApp f a = do
      ef <- rec f
      case ef of
        Right (FunVal f) -> rec (f a)
        _                -> pure $ Left "Applies not a function"

    evalLam v body = undefined

    evalIf c t e = do
      eRes <- rec c
      case eRes of
        Right (PrimVal res) | Just b <- fromPrimBool res -> if b then rec t else rec e
        Right _  -> pure $ Left "Not a boolean"
        Left err -> pure $ Left err

    evalFix e = rec (App e (Fix e))

    evalLet v e body = rec (App (Lam v body) e)

    rec = eval' env
