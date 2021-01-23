module Eval (eval, norm) where

import Data.Char (isDigit)
import Data.List
import Types

eval env (If x y z) = eval env $ case eval env x of
  Bool True -> y
  Bool False -> z
eval env (App m a) =
  let m' = eval env m
   in case m' of
        Lam (v, _) f ->
          let beta (Atom s)
                | s == v = a
                | otherwise = Atom s
              beta (Lam (s, t) m)
                | s == v = Lam (s, t) m
                | s `elem` fvs =
                  let s1 = newName s fvs
                   in Lam (s1, t) $ beta $ rename s s1 m
                | otherwise = Lam (s, t) (beta m)
              beta (App m n) = App (beta m) (beta n)
              beta (If x y z) = If (beta x) (beta y) (beta z)
              fvs = fv [] a
           in eval env $ beta f
        Atom "not" -> case eval env a of
          Bool True -> Bool False
          Bool False -> Bool True
        Atom "negate" -> case eval env a of
          Int x -> Int $ negate x
        App (Atom "add") b -> Int (aInt + bInt)
          where
            Int aInt = eval env a
            Int bInt = eval env b
        _ -> App m' a
eval env term@(Atom v) | Just x <- lookup v env = eval env x
eval _ term = term

fv vs (Atom s)
  | s `elem` vs = []
  | otherwise = [s]
fv vs (Lam (s, _) f) = fv (s : vs) f
fv vs (App x y) = fv vs x `union` fv vs y
fv vs (If x y z) = fv vs x `union` fv vs y `union` fv vs z

newName x ys = head $ filter (`notElem` ys) $ (s ++) . show <$> [1 ..]
  where
    s = dropWhileEnd isDigit x

rename x x1 term = case term of
  Atom s
    | s == x -> Atom x1
    | otherwise -> term
  Lam (s, t) b
    | s == x -> term
    | otherwise -> Lam (s, t) (rec b)
  App a b -> App (rec a) (rec b)
  where
    rec = rename x x1

norm env term = case eval env term of
  Atom v -> Atom v
  Lam v m -> Lam v (rec m)
  App m n -> App (rec m) (rec n)
  If x y z -> If (rec x) (rec y) (rec z)
  where
    rec = norm env
