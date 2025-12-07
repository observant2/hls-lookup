module Test.ModuleLookup (tests) where

import ModuleLookup
import Test.Tasty
import Test.Tasty.HUnit
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

tests :: TestTree
tests = testGroup "ModuleLookup"
  [ testModuleNameToPath
  , testFindModuleFile
  ]

testModuleNameToPath :: TestTree
testModuleNameToPath = testGroup "moduleNameToPath"
  [ testCase "simple module" $
      moduleNameToPath "Data.Map" @?= "Data/Map.hs"

  , testCase "nested module" $
      moduleNameToPath "Network.HTTP.Simple" @?= "Network/HTTP/Simple.hs"

  , testCase "single component" $
      moduleNameToPath "Prelude" @?= "Prelude.hs"

  , testCase "deeply nested module" $
      moduleNameToPath "Control.Monad.Trans.Reader" @?= "Control/Monad/Trans/Reader.hs"
  ]

testFindModuleFile :: TestTree
testFindModuleFile = testGroup "findModuleFile"
  [ testCase "finds module in src directory" $ do
      withSystemTempDirectory "hls-lookup-test" $ \tmpDir -> do
        let modulePath = tmpDir </> "src" </> "Data" </> "Foo.hs"
        createDirectoryIfMissing True (tmpDir </> "src" </> "Data")
        writeFile modulePath "module Data.Foo where"

        result <- findModuleFile tmpDir "Data.Foo"
        result @?= Just modulePath

  , testCase "finds module in lib directory" $ do
      withSystemTempDirectory "hls-lookup-test" $ \tmpDir -> do
        let modulePath = tmpDir </> "lib" </> "Bar.hs"
        createDirectoryIfMissing True (tmpDir </> "lib")
        writeFile modulePath "module Bar where"

        result <- findModuleFile tmpDir "Bar"
        result @?= Just modulePath

  , testCase "finds module in root directory" $ do
      withSystemTempDirectory "hls-lookup-test" $ \tmpDir -> do
        let modulePath = tmpDir </> "Baz.hs"
        writeFile modulePath "module Baz where"

        result <- findModuleFile tmpDir "Baz"
        result @?= Just modulePath

  , testCase "returns Nothing for non-existent module" $ do
      withSystemTempDirectory "hls-lookup-test" $ \tmpDir -> do
        result <- findModuleFile tmpDir "NonExistent.Module"
        result @?= Nothing

  , testCase "prefers src over root" $ do
      withSystemTempDirectory "hls-lookup-test" $ \tmpDir -> do
        let srcPath = tmpDir </> "src" </> "Test.hs"
        let rootPath = tmpDir </> "Test.hs"
        createDirectoryIfMissing True (tmpDir </> "src")
        writeFile srcPath "module Test where -- src"
        writeFile rootPath "module Test where -- root"

        result <- findModuleFile tmpDir "Test"
        result @?= Just srcPath
  ]
