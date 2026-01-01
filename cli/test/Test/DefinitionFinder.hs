module Test.DefinitionFinder (tests) where

import DefinitionFinder
import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Data.Text (Text)
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "DefinitionFinder"
  [ testTypeSignature
  , testFunctionDefinition
  , testDataConstructor
  , testTypeDefinition
  , testNewtypeDefinition
  , testClassDefinition
  , testMultipleOccurrences
  , testNotFound
  ]

-- Helper to create a temp file with content and find a symbol
findInTempFile :: Text -> Text -> IO (Maybe DefinitionLocation)
findInTempFile content symbolName = do
  withSystemTempDirectory "hls-lookup-test" $ \tmpDir -> do
    let filePath = tmpDir </> "Test.hs"
    writeFile filePath (T.unpack content)
    findDefinition filePath symbolName

testTypeSignature :: TestTree
testTypeSignature = testGroup "type signature"
  [ testCase "simple type signature" $ do
      let content = T.unlines
            [ "module Test where"
            , ""
            , "httpLBS :: Request -> IO Response"
            , "httpLBS = undefined"
            ]
      result <- findInTempFile content "httpLBS"
      result @?= Just (DefinitionLocation 3 1)

  , testCase "type signature with indentation" $ do
      let content = T.unlines
            [ "module Test where"
            , ""
            , "  parseJSON :: Value -> Parser a"
            , "  parseJSON = undefined"
            ]
      result <- findInTempFile content "parseJSON"
      result @?= Just (DefinitionLocation 3 3)
  ]

testFunctionDefinition :: TestTree
testFunctionDefinition = testGroup "function definition"
  [ testCase "simple function definition" $ do
      let content = T.unlines
            [ "module Test where"
            , ""
            , "add x y = x + y"
            ]
      result <- findInTempFile content "add"
      result @?= Just (DefinitionLocation 3 1)

  , testCase "function definition with guards" $ do
      let content = T.unlines
            [ "module Test where"
            , ""
            , "factorial n"
            , "  | n <= 1 = 1"
            , "  | otherwise = n * factorial (n - 1)"
            ]
      result <- findInTempFile content "factorial"
      result @?= Just (DefinitionLocation 3 1)

  , testCase "function with no parameters" $ do
      let content = T.unlines
            [ "module Test where"
            , ""
            , "myConstant = 42"
            ]
      result <- findInTempFile content "myConstant"
      result @?= Just (DefinitionLocation 3 1)
  ]

testDataConstructor :: TestTree
testDataConstructor = testGroup "data constructor"
  [ testCase "data type with constructors" $ do
      let content = T.unlines
            [ "module Test where"
            , ""
            , "data Result = Success | Failure"
            ]
      result <- findInTempFile content "Success"
      result @?= Just (DefinitionLocation 3 1)

  , testCase "data constructor on separate line" $ do
      let content = T.unlines
            [ "module Test where"
            , ""
            , "data HttpException"
            , "  = ConnectionTimeout"
            , "  | InvalidResponse"
            , "  | TooManyRedirects"
            ]
      result <- findInTempFile content "InvalidResponse"
      result @?= Just (DefinitionLocation 5 3)

  , testCase "data constructor with fields" $ do
      let content = T.unlines
            [ "module Test where"
            , ""
            , "data Person = Person { name :: String, age :: Int }"
            ]
      result <- findInTempFile content "Person"
      result @?= Just (DefinitionLocation 3 1)
  ]

testTypeDefinition :: TestTree
testTypeDefinition = testGroup "type definition"
  [ testCase "type alias" $ do
      let content = T.unlines
            [ "module Test where"
            , ""
            , "type String = [Char]"
            ]
      result <- findInTempFile content "String"
      result @?= Just (DefinitionLocation 3 1)

  , testCase "type alias with parameters" $ do
      let content = T.unlines
            [ "module Test where"
            , ""
            , "type Map k v = Data.Map.Map k v"
            ]
      result <- findInTempFile content "Map"
      result @?= Just (DefinitionLocation 3 1)
  ]

testNewtypeDefinition :: TestTree
testNewtypeDefinition = testGroup "newtype definition"
  [ testCase "simple newtype" $ do
      let content = T.unlines
            [ "module Test where"
            , ""
            , "newtype UserId = UserId Int"
            ]
      result <- findInTempFile content "UserId"
      result @?= Just (DefinitionLocation 3 1)

  , testCase "newtype with record syntax" $ do
      let content = T.unlines
            [ "module Test where"
            , ""
            , "newtype Wrapper = Wrapper { unwrap :: String }"
            ]
      result <- findInTempFile content "Wrapper"
      result @?= Just (DefinitionLocation 3 1)
  ]

testClassDefinition :: TestTree
testClassDefinition = testGroup "class definition"
  [ testCase "simple class" $ do
      let content = T.unlines
            [ "module Test where"
            , ""
            , "class Monad m where"
            , "  return :: a -> m a"
            , "  (>>=) :: m a -> (a -> m b) -> m b"
            ]
      result <- findInTempFile content "Monad"
      result @?= Just (DefinitionLocation 3 1)

  , testCase "class with context" $ do
      let content = T.unlines
            [ "module Test where"
            , ""
            , "class (Eq a) => Ord a where"
            , "  compare :: a -> a -> Ordering"
            ]
      result <- findInTempFile content "Ord"
      result @?= Just (DefinitionLocation 3 1)
  ]

testMultipleOccurrences :: TestTree
testMultipleOccurrences = testGroup "multiple occurrences"
  [ testCase "returns first definition (type signature)" $ do
      let content = T.unlines
            [ "module Test where"
            , ""
            , "foo :: Int"
            , "foo = 42"
            , ""
            , "bar = foo + 1"  -- usage, not definition
            ]
      result <- findInTempFile content "foo"
      result @?= Just (DefinitionLocation 3 1)
  ]

testNotFound :: TestTree
testNotFound = testGroup "not found"
  [ testCase "symbol not in file" $ do
      let content = T.unlines
            [ "module Test where"
            , ""
            , "foo = 42"
            ]
      result <- findInTempFile content "bar"
      result @?= Nothing

  , testCase "symbol as substring" $ do
      let content = T.unlines
            [ "module Test where"
            , ""
            , "foobar = 42"
            ]
      result <- findInTempFile content "foo"
      result @?= Nothing
  ]
