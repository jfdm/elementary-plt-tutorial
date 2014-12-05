module ArithLang

import PlangTheory

-- ------------------------------------------------------------------- [ Types ]
data Ty = TyValue

instance Eq Ty where
  (==) TyValue TyValue = True

-- ----------------------------------------------------------- [ Arith Context ]

example_context : Context Ty
example_context = [("foo", TyValue), ("bar", TyValue)]

-- TODO sort extending context.
instance TypingContext Ty where
  remove = deleteBy (\(x,y),(c,d) => x==c)
  extend e es = e :: (remove e es)
  lookup = List.lookup

-- Examples
ctxt : Context Ty
ctxt = [("foo", TyValue), ("g", TyValue)]

env' : TypingContext Ty => Context Ty
env' = extend ("g", TyValue) []

env'' : TypingContext Ty => Context Ty
env'' = extend ("f", TyValue) ctxt

-- ------------------------------------------------------------------- [ Rules ]

data Arith : Context Ty -> Ty -> Type where
  Val : Int -> Arith g TyValue
  Var : (n : String) -> Arith g TyValue
  Neg : Arith g TyValue -> Arith g TyValue
  Add : Arith g TyValue -> Arith g TyValue -> Arith g TyValue
  Sub : Arith g TyValue -> Arith g TyValue -> Arith g TyValue
  Div : Arith g TyValue -> Arith g TyValue -> Arith g TyValue
  Mul : Arith g TyValue -> Arith g TyValue -> Arith g TyValue


-- ------------------------------------------------------------------ [ Interp ]

interpTy : Ty -> Type
interpTy TyValue = Int

data Env : Context Ty -> Type where
  Nil  : Env Nil
  (::) : (e : (String, Int)) -> Env g ->  Env (extend (fst e, TyValue) g)

getValue : String -> Env g -> Maybe Int
getValue n Nil           = Nothing
getValue n ((a,v) :: xs) = case n == a of
  True => Just v
  False => getValue n xs

interp : Env g -> Arith g TyValue -> Int
interp env (Val x)    = x
interp env (Var n)    = case getValue n env of
    Just x => x
    Nothing => 0
interp env (Neg x)    = (-1) * (interp env x)
interp env (Add x y)  = (interp env x) + (interp env y)
interp env (Sub x y)  = (interp env x) - (interp env y)
interp env (Div x y)  = (interp env x) `div` (interp env y)
interp env (Mul x y)  = (interp env x) * (interp env y)

env : Env [("x", TyValue)]
env = [("x", 3)]
