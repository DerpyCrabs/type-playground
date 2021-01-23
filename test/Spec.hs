import Eval
import Parser
import Test.Hspec
import TypeSystem
import Types

main :: IO ()
main = hspec $ do
  describe "Parser" parsingTests
  describe "TypeSystem" typeSystemTests

parsingTests =
  let test = testTable runParse
   in do
        it "parses primitives" $
          test
            [ ("5", Right $ Int 5),
              ("false", Right $ Bool False),
              ("true", Right $ Bool True)
            ]
        it "parses if" $
          test
            [ ("(if true 5 3)", Right $ If (Bool True) (Int 5) (Int 3)),
              ("(if (not false) (add 5 3) (negate 3))", Right $ If (App (Atom "not") (Bool False)) (App (App (Atom "add") (Int 5)) (Int 3)) (App (Atom "negate") (Int 3)))
            ]
        it "parses function definition" $
          test
            [ ("(fn [a Int] a)", Right $ Lam ("a", TInt) (Atom "a")),
              ("(fn [a Int] (negate a))", Right $ Lam ("a", TInt) (App (Atom "negate") (Atom "a")))
            ]

typeSystemTests =
  let test = testTable runTypeSystem
   in do
        it "handles primitives" $
          test
            [ ("5", Right TInt),
              ("false", Right TBool),
              ("true", Right TBool)
            ]
        it "handles functions" $
          test
            [ ("(fn [a Int] a)", Right (TFunc TInt TInt))
            ]
        it "handles application" $
          test
            [ ("(negate 5)", Right TInt),
              ("(add 5 3)", Right TInt),
              ("(add 5)", Right (TFunc TInt TInt)),
              ("(add false)", Left $ Default "")
            ]
        it "handles if" $
          test
            [ ("(if true 5 4)", Right TInt),
              ("(if 5 true false)", Left $ Default "")
            ]

runParse :: String -> IO (ThrowsError Term)
runParse = return . readExpr "test"

runTypeSystem :: String -> IO (ThrowsError Type)
runTypeSystem s = return $ do
  term <- readExpr "test" s
  let t = typeOf [] term
  case t of
    Nothing -> Left $ Default ""
    Just t -> Right t

testTable :: (Show b, Eq b) => (a -> IO b) -> [(a, b)] -> IO ()
testTable _ [] = return ()
testTable runTest ((input, expected) : tests) = (runTest input `shouldReturn` expected) >> testTable runTest tests
