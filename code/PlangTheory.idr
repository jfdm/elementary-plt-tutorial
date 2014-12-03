module PlangTheory

Context : Type -> Type
Context ty = List (String, ty)


class (Eq ty) => TypingContext ty where
  remove : (String, ty) -> Context ty -> Context ty
  extend : (String, ty) -> Context ty -> Context ty
  lookup : String       -> Context ty -> Maybe ty

  remove e es = deleteBy (\(x,y),(c,d) => x==c) e es
  extend e es = e :: (remove e es)
  lookup = List.lookup
