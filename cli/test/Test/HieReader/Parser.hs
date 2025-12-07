module Test.HieReader.Parser (tests) where

import HieReader.Parser
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "HieReader.Parser"
  [ testParseUnitId
  ]

testParseUnitId :: TestTree
testParseUnitId = testGroup "parseUnitId"
  [ testCase "simple package with version" $
      parseUnitId "aeson-2.2.3.0" @?= (Just "aeson", Just "2.2.3.0")

  , testCase "package with hash suffix" $
      parseUnitId "aeson-2.2.3.0-abcd1234ef567890abcd1234ef567890abcd1234ef567890abcd1234ef567890"
        @?= (Just "aeson", Just "2.2.3.0")

  , testCase "package with library qualifier" $
      parseUnitId "http-conduit-2.3.8:lib:http-conduit"
        @?= (Just "http-conduit", Just "2.3.8")

  , testCase "package with library qualifier and hash" $
      parseUnitId "bytestring-0.12.1.0:lib:bytestring+abcd1234ef567890abcd1234ef567890abcd1234ef567890abcd1234ef567890"
        @?= (Just "bytestring", Just "0.12.1.0")

  , testCase "multi-component package name" $
      parseUnitId "http-conduit-2.3.8.1"
        @?= (Just "http-conduit", Just "2.3.8.1")

  , testCase "package name with multiple dashes" $
      parseUnitId "network-http-client-3.2.1.0"
        @?= (Just "network-http-client", Just "3.2.1.0")

  , testCase "package without version" $
      parseUnitId "mypackage"
        @?= (Just "mypackage", Nothing)

  , testCase "package with short hash (not filtered)" $
      parseUnitId "aeson-2.2.3.0-abc123"
        @?= (Just "aeson", Just "2.2.3.0")

  , testCase "empty string" $
      parseUnitId ""
        @?= (Nothing, Nothing)

  , testCase "only version numbers" $
      parseUnitId "1.2.3.4"
        @?= (Nothing, Nothing)

  , testCase "complex real-world example" $
      parseUnitId "ghc-9.12.20241109-f2ff6e901c3280e8ecd5aefb01c6bb2c26e6b91e7e4e6e0e6e6e6e6e6e6e6e6e"
        @?= (Just "ghc", Just "9.12.20241109")
  ]
