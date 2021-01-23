module Parser (readExpr) where

import Control.Monad.Except
import Data.Maybe (catMaybes)
import Text.Megaparsec hiding (spaces)
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L
import Types

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?^_"

spaces :: Parser ()
spaces = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "/*" "*/")

lbracket = lexeme $ char '['

rbracket = lexeme $ char ']'

lparen = lexeme $ char '('

rparen = lexeme $ char ')'

lcbracket = lexeme $ char '{'

rcbracket = lexeme $ char '}'

fnArrow = lexeme $ string "->"

dottedListSeparator = lexeme $ char '.'

-- parseString :: Parser Term
-- parseString = char '"' >> String <$> manyTill L.charLiteral (char '"')

-- parseUnit :: Parser Term
-- parseUnit = L.symbol spaces "unit" >> return Unit

parseAtom :: Parser Term
parseAtom = do
  pos <- getSourcePos
  first <- letterChar <|> symbol
  rest <- many (alphaNumChar <|> symbol <|> char '.')
  let atom = first : rest
  return $ case atom of
    "true" -> Bool True
    "false" -> Bool False
    _ -> Atom atom

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

-- parseUnicodeCharacter :: Parser Char
-- parseUnicodeCharacter = toEnum . read <$> count 4 digitChar

-- parseCharacter :: Parser Term
-- parseCharacter =
--   Character
--     <$> ( char '\\'
--             >> try (L.symbol spaces "newline" >> return '\n')
--             <|> try (L.symbol spaces "space" >> return ' ')
--             <|> try (L.symbol spaces "tab" >> return '\t')
--             <|> try (L.symbol spaces "formfeed" >> return '\f')
--             <|> try (L.symbol spaces "backspace" >> return '\b')
--             <|> try (L.symbol spaces "return" >> return '\r')
--             <|> try parseUnicodeCharacter
--             <|> alphaNumChar
--         )

parseBinary :: Parser Integer
parseBinary = char '0' >> char 'b' >> L.binary

parseOctal :: Parser Integer
parseOctal = char '0' >> char 'o' >> L.octal

parseHexadecimal :: Parser Integer
parseHexadecimal = char '0' >> char 'x' >> L.hexadecimal

parseDecimal :: Parser Integer
parseDecimal = L.decimal

parseInteger :: Parser Term
parseInteger = Int <$> L.signed (return ()) (try parseBinary <|> try parseOctal <|> try parseHexadecimal <|> parseDecimal)

-- parseFloat :: Parser Value
-- parseFloat = Float <$> L.signed (return ()) L.float

parseExprOrSkip = try skipExpr <|> (Just <$> parseExpr)
  where
    parseExprOrSkip = try skipExpr <|> (Just <$> parseExpr)
    skipExpr = do
      char '#' >> char '_'
      _ <- parseExpr
      return Nothing

parseCall :: Parser Term
parseCall = do
  lparen
  v <- try parseFnInner <|> try parseIfInner <|> parseCallInner
  rparen
  return v

parseCallInner :: Parser Term
parseCallInner = do
  exprs <- catMaybes <$> sepBy parseExprOrSkip spaces
  return $ constructApp exprs
  where
    constructApp [t] = t
    constructApp t = App (constructApp (init t)) (last t)

parseIfInner :: Parser Term
parseIfInner = do
  _ <- lexeme $ string "if"
  liftM3 If parseExpr parseExpr parseExpr

parseFnInner :: Parser Term
parseFnInner = do
  lexeme $ string "fn"
  lbracket
  (Atom arg) <- parseExpr
  argType <- parseType
  rbracket
  Lam (arg, argType) <$> parseExpr

parseType :: Parser Type
parseType = parseIntType <|> parseBoolType <|> parseFnType

parseIntType = string "Int" >> return TInt

parseBoolType = string "Bool" >> return TBool

parseFnType = do
  lparen
  arg <- lexeme parseType
  fnArrow
  ret <- lexeme parseType
  return $ TFunc arg ret

-- parseListInner :: Parser Value
-- parseListInner = getSourcePos >>= \pos -> List (Just pos) . catMaybes <$> sepBy parseExprOrSkip spaces

-- parseDottedListInner :: Parser Value
-- parseDottedListInner = do
--   pos <- getSourcePos
--   head <- manyTill parseExpr dottedListSeparator
--   DottedList (Just pos) head <$> parseExpr

-- parseList :: Parser Value
-- parseList = do
--   lbracket
--   x <- lexeme (try parseDottedListInner <|> parseListInner)
--   rbracket
--   return x

-- parseMapInner :: Parser Value
-- parseMapInner = Map . catMaybes <$> sepBy parseExprOrSkip spaces

-- parseMap :: Parser Value
-- parseMap = do
--   lcbracket
--   map <- lexeme parseMapInner
--   rcbracket
--   return map

-- parseQuoted :: Parser Value
-- parseQuoted = do
--   pos <- getSourcePos
--   char '\''
--   x <- parseExpr
--   return $ Call [Atom (Just pos) "quote", x]

-- parseUnquoted :: Parser Value
-- parseUnquoted = do
--   pos <- getSourcePos
--   char '~'
--   x <- parseExpr
--   return $ Call [Atom (Just pos) "unquote", x]

-- parseUnquoteSplicing :: Parser Value
-- parseUnquoteSplicing = do
--   pos <- getSourcePos
--   char '~' >> char '@'
--   x <- parseExpr
--   return $ Call [Atom (Just pos) "unquote-splicing", x]

-- parseLambdaShorthand :: Parser Value
-- parseLambdaShorthand = do
--   pos <- getSourcePos
--   char '#' >> lparen
--   inner <- lexeme parseCallInner
--   rparen
--   return $ Call (Atom Nothing "fn" : DottedList Nothing [] (Atom Nothing "%&") : [inner])

-- parseLambdaShorthandArgs =
--   try parseSingle <|> parseNth
--   where
--     parseSingle = do
--       char '%' >> char '%'
--       return (Call [Atom Nothing "car", Atom Nothing "%&"])
--     parseNth :: Parser Value
--     parseNth = do
--       char '%'
--       n <- digitChar
--       return (Call [Atom Nothing "nth", Integer (read [n] - 1), Atom Nothing "%&"])

parseExpr :: Parser Term
parseExpr =
  lexeme $
    -- parseString
    -- <|> parseCharacter
    -- <|> try parseFloat
    try parseInteger
      -- <|> try parseLambdaShorthandArgs
      -- <|> parseUnit
      <|> parseAtom
      -- <|> parseLambdaShorthand
      <|> parseCall

-- <|> parseMap
-- <|> parseList

readOrThrow :: Parser a -> String -> String -> ThrowsError a
readOrThrow parser file input = case parse parser file input of
  Left err -> throwError $ Parsing err
  Right val -> return val

readExpr = readOrThrow parseExpr
