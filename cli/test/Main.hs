module Main (main) where

import Test.Tasty
import qualified Test.HieReader.Parser as Parser
import qualified Test.ModuleLookup as ModuleLookup
import qualified Test.DefinitionFinder as DefinitionFinder

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "hls-lookup tests"
  [ Parser.tests
  , ModuleLookup.tests
  , DefinitionFinder.tests
  ]
