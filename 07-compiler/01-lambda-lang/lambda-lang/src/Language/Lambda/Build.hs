module Language.Lambda.Build(
  prim,
  var,
  app,
  lam,
  iff,
  loop,
  let',
) where

import Control.Monad.Identity
import Data.Boolean
import Language.Lambda.Types

newtype Expr p v a = Expr (Core p v)

prim :: p -> Expr p v a
prim p = Expr (Prim p)

var :: v -> Expr p v a
var v = Expr (Var v)

app :: Expr p v (a -> b) -> Expr p v a -> Expr p v b
app (Expr a) (Expr b) = Expr (App a b)

lam :: Expr p v a -> Expr p v b -> Expr p v (a -> b)
lam (Expr (Var v)) (Expr b) = Expr (Lam v b)

iff :: Expr p v Bool -> Expr p v a -> Expr p v a -> Expr p v a
iff (Expr c) (Expr t) (Expr e) = Expr (If c t e)

loop :: Expr p v (a -> a) -> Expr p v a
loop (Expr a) = Expr (Fix a)

let' :: Expr p v a -> Expr p v a -> Expr p v b -> Expr p v b
let' (Expr (Var v)) (Expr e) (Expr body) = Expr (Let v e body)

op1 :: (Core p v -> PrimOp p (Core p v)) -> Expr p v a -> Expr p v b
op1 op (Expr a) = Expr (Op (op a))

op2 :: (Core p v -> Core p v -> PrimOp p (Core p v)) -> Expr p v a -> Expr p v b -> Expr p v c
op2 op (Expr a) (Expr b) = Expr (Op (op a b))

-------------------------------------------------

data P = PInt Int | PBool Bool
type V = String
type E a = Expr P V a

data Op a
  = Not a | And a a | Or a a
  | Neg a | Add a a | Mul a a
  | Eq a a | Neq a a | Lt a a | Gt a a | Leq a a | Geq a a
  deriving (Functor, Foldable, Traversable)

instance IsPrim P where
  type PrimOp P = Op
  type EvalMonad P = Identity

  evalPrim expr = pure $ case expr of
    -- boolean operators
    Not (PBool b) -> Right $ PBool (not b)
    And (PBool a) (PBool b) -> Right $ PBool (a && b)
    Or (PBool a) (PBool b) -> Right $ PBool (a || b)
    -- numeric operators
    Neg (PInt a) -> Right $ PInt (negate a)
    Add (PInt a) (PInt b) -> Right $ PInt (a + b)
    Mul (PInt a) (PInt b) -> Right $ PInt (a * b)
    -- compare
    Eq (PInt a) (PInt b) -> Right $ PBool (a == b)
    Eq (PBool a) (PBool b) -> Right $ PBool (a == b)
    Neq (PInt a) (PInt b) -> Right $ PBool (a /= b)
    Neq (PBool a) (PBool b) -> Right $ PBool (a /= b)
    Lt (PInt a) (PInt b) -> Right $ PBool (a < b)
    Lt (PBool a) (PBool b) -> Right $ PBool (a < b)
    Gt (PInt a) (PInt b) -> Right $ PBool (a > b)
    Gt (PBool a) (PBool b) -> Right $ PBool (a > b)
    Leq (PInt a) (PInt b) -> Right $ PBool (a <= b)
    Leq (PBool a) (PBool b) -> Right $ PBool (a <= b)
    Geq (PInt a) (PInt b) -> Right $ PBool (a >= b)
    Geq (PBool a) (PBool b) -> Right $ PBool (a >= b)
    _  -> Left "Invalid prim op"

  fromPrimBool = \case
    PBool b -> Just b
    _       -> Nothing

int :: Int -> E Int
int = prim . PInt

bool :: Bool -> E Bool
bool = prim . PBool

instance Num (E Int) where
  fromInteger = int . fromInteger
  (+) = op2 Add
  (*) = op2 Mul
  negate = op1 Neg
  abs = error "abs not implemented"
  signum = error "signum not implemented"

instance Boolean (E Bool) where
  true = bool True
  false = bool False
  notB = op1 Not
  (&&*) = op2 And
  (||*) = op2 Or

type instance BooleanOf (E a) = E Bool

instance IfB (E a) where
  ifB = iff

instance EqB (E a) where
  (==*) = op2 Eq
  (/=*) = op2 Neq

instance OrdB (E a) where
  (<*) = op2 Lt
  (>*) = op2 Gt
  (<=*) = op2 Leq
  (>=*) = op2 Geq

expr1 :: E Int
expr1 = 1 + 2 * 3

expr2 :: E Bool
expr2 = expr1 >* 100 ||* (2 + 2 ==* (4 :: E Int))

{-

example :: E (Int -> Int)
example = lam n $ loop $ lam x $
  ifB (x <=* 0)
    x

  where
    x = var "x"
    n = var "n"

JSON lang example,
add lists to Arithm/Bool
-}

