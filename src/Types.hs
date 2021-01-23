module Types (ThrowsError, Parser, Term (..), Error (..), Type (..)) where

import Data.Void
import Text.Megaparsec

type ThrowsError = Either Error

data Error
  = TypeMismatch Type Type
  | Default String
  | Parsing (ParseErrorBundle String Void)
  deriving (Eq)

type Parser = Parsec Void String

data Type = TInt | TBool | TFunc Type Type deriving (Eq)

data Term = Atom String | App Term Term | Lam (String, Type) Term | If Term Term Term | Bool Bool | Int Integer deriving (Eq)

instance Show Error where
  show (TypeMismatch expected found) =
    "Invalid type: expected " ++ show expected
      ++ " but found "
      ++ show found
  show (Parsing parseErr) = errorBundlePretty parseErr
  show (Default err) = err

instance Show Type where
  show TInt = "Integer"
  show TBool = "Bool"
  show (TFunc arg@(TFunc _ _) ret) = "(" ++ show arg ++ ") -> " ++ show ret
  show (TFunc arg ret) = show arg ++ " -> " ++ show ret

instance Show Term where
  show (Atom s) = s
  show app@(App _ _) = "(" ++ showApp app ++ ")"
    where
      showApp (App app2@(App _ _) t3) = showApp app2 ++ " " ++ show t3
      showApp (App t1 t2) = show t1 ++ " " ++ show t2
  show (Lam (arg, argType) body) = "(fn [" ++ show arg ++ " " ++ show argType ++ "] " ++ show body ++ ")"
  show (If pred conseq alt) = "(if " ++ show pred ++ " " ++ show conseq ++ " " ++ show alt ++ ")"
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int i) = show i
