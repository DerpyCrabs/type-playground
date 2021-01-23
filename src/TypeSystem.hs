module TypeSystem (typeOf) where

import Types

typeOf :: [(String, Type)] -> Term -> Maybe Type
typeOf gamma t = case t of
  Atom "negate" -> Just (TFunc TInt TInt)
  Atom "add" -> Just (TFunc TInt (TFunc TInt TInt))
  Atom "not" -> Just (TFunc TBool TBool)
  Bool _ -> Just TBool
  Int _ -> Just TInt
  Atom s -> lookup s gamma
  App x y -> do
    tx <- rec x
    ty <- rec y
    case tx of
      TFunc ty' tz | ty == ty' -> pure tz
      _ -> Nothing
  Lam (x, t) y -> do
    u <- typeOf ((x, t) : gamma) y
    pure $ TFunc t u
  If x y z ->
    if rec x /= Just TBool
      then Nothing
      else do
        ty <- rec y
        tz <- rec z
        if ty == tz then pure ty else Nothing
  where
    rec = typeOf gamma
