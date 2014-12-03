module TypedArith

{-
This module covers:

1. Grammars
2. Basic Type Theory
  1. Types
  2. Simple Typing Contexts i.e. Gamma
  3. Typing Rules.
3. Denotational Semantics.

-}

-- ------------------------------------------------------------------- [ Types ]
data ArithTy = TyValue

interpTy : ArithTy -> Type
interpTy TyValue = Int

-- ------------------------------------------------ [ Grammar and Typing Rules ]

data Arith : ArithTy -> Type where
  Val : Int                            -> Arith TyValue
  Neg : Arith TyValue                  -> Arith TyValue
  Add : Arith TyValue -> Arith TyValue -> Arith TyValue
  Sub : Arith TyValue -> Arith TyValue -> Arith TyValue
  Div : Arith TyValue -> Arith TyValue -> Arith TyValue
  Mul : Arith TyValue -> Arith TyValue -> Arith TyValue

 ------------------------------------------------------------------ [ Interp ]
interp : Arith t -> interpTy t
interp (Val x)   = x
interp (Neg x)   = (-1) * (interp x)
interp (Add x y) = (interp x) + (interp y)
interp (Sub x y) = (interp x) - (interp y)
interp (Div x y) = (interp x) `div` (interp y)
interp (Mul x y) = (interp x) * (interp y)

eg : Arith TyValue
eg = Add (Val 1) (Val 3)
