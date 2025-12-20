module HieReader
  ( loadHieFile,
    findHieFile,
    inspectHieFile,
    findSymbolAt,
  )
where

import Data.Map qualified as Map
import GHC.Iface.Ext.Binary (HieFileResult (..), readHieFile)
import GHC.Iface.Ext.Types (HieFile (hie_asts, hie_hs_file, hie_module), getAsts)
import GHC.Types.Name.Cache (initNameCache)
import GHC.Unit.Module (moduleName, moduleNameString)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory, canonicalizePath)
import System.FilePath (takeExtension, takeDirectory, (</>), (<.>), splitPath, joinPath)
import Control.Monad (filterM, when)
import Control.Exception (catch, SomeException)
import SymbolLookup
import LookupTypes

-- | Load a .hie file from FilePath
loadHieFile :: FilePath -> IO HieFile
loadHieFile hiePath = do
  nameCache <- initNameCache 'z' []
  result <- readHieFile nameCache hiePath
  return result.hie_file_result

-- | Find the package root by traversing up from a directory looking for a .cabal file
-- Returns Nothing if no .cabal file is found (reaches filesystem root)
findPackageRoot :: FilePath -> IO (Maybe FilePath)
findPackageRoot dir = do
  -- Try to list directory, handle failures by moving to parent
  mEntries <- (Just <$> listDirectory dir) `catch` \(_ :: SomeException) -> pure Nothing

  case mEntries of
    Just entries -> do
      let hasCabalFile = any (\f -> takeExtension f == ".cabal") entries
      if hasCabalFile
        then pure (Just dir)
        else tryParent
    Nothing -> pure Nothing
  where
    tryParent = do
      let parentDir = takeDirectory dir
      -- Stop if we've reached the root (parent == current)
      if parentDir == dir
        then pure Nothing
        else findPackageRoot parentDir

-- | Find .hie file for a source file by searching recursively in .hie/ directory
-- Verifies the match by checking the hie_hs_file field inside the HIE file
-- For multi-project setups, finds the package root by looking for .cabal file
findHieFile :: FilePath -> IO (Maybe FilePath)
findHieFile srcFile = do
  canonicalSrc <- canonicalizePath srcFile
  putStrLn $ "DEBUG: Canonical source: " ++ canonicalSrc

  -- Find the package root (directory containing .cabal file)
  mPackageRoot <- findPackageRoot (takeDirectory canonicalSrc)
  putStrLn $ "DEBUG: Package root: " ++ show mPackageRoot
  case mPackageRoot of
    Nothing -> return Nothing  -- No .cabal file found
    Just packageRoot -> do
      let hieFolder = packageRoot </> ".hie"
      putStrLn $ "DEBUG: HIE folder: " ++ hieFolder
      hieExists <- doesDirectoryExist hieFolder
      putStrLn $ "DEBUG: HIE folder exists: " ++ show hieExists
      if not hieExists
        then return Nothing
        else do
          hieFiles <- findHieFilesRecursive hieFolder
          putStrLn $ "DEBUG: Found " ++ show (length hieFiles) ++ " HIE files"
          findMatchingHie canonicalSrc hieFiles
  where
    findHieFilesRecursive :: FilePath -> IO [FilePath]
    findHieFilesRecursive dir = do
      entries <- listDirectory dir
      let fullPaths = map (dir </>) entries

      dirs <- filterM doesDirectoryExist fullPaths
      files <- filterM doesFileExist fullPaths

      let hieFiles = filter (\f -> takeExtension f == ".hie") files

      -- Recursively search subdirectories
      subHieFiles <- concat <$> mapM findHieFilesRecursive dirs

      return (hieFiles ++ subHieFiles)

    findMatchingHie :: FilePath -> [FilePath] -> IO (Maybe FilePath)
    findMatchingHie _ [] = do
      putStrLn "DEBUG: No matching HIE file found after checking all files"
      return Nothing
    findMatchingHie canonical (hiePath:rest) = do
      -- Load the HIE file and check if it matches
      hieFile <- loadHieFile hiePath
      canonicalHieSrc <- canonicalizePath hieFile.hie_hs_file

      -- Debug: show first few comparisons
      when (length rest > 155) $
        putStrLn $ "DEBUG: Comparing '" ++ canonicalHieSrc ++ "' with '" ++ canonical ++ "'"

      if canonicalHieSrc == canonical
        then do
          putStrLn $ "DEBUG: Match found! " ++ hiePath
          return (Just hiePath)
        else findMatchingHie canonical rest

-- | Test function to inspect what's in a .hie file
inspectHieFile :: FilePath -> IO ()
inspectHieFile hiePath = do
  hieFile <- loadHieFile hiePath
  putStrLn $ "Module: " ++ (moduleNameString . moduleName) hieFile.hie_module
  putStrLn $ "Source file: " ++ hieFile.hie_hs_file
  putStrLn $ "AST entries: " ++ show (Map.size $ getAsts hieFile.hie_asts)

-- | Test function to find symbols at a position
findSymbolAt :: FilePath -> Int -> Int -> IO ()
findSymbolAt hiePath line col = do
  hieFile <- loadHieFile hiePath
  let symbols = getSymbolsAtPosition hieFile line col
  putStrLn $ "Symbols at " ++ show line ++ ":" ++ show col ++ ":"
  mapM_ printSymbol symbols
  where
    printSymbol sym = do
      putStrLn $ "  Name: " ++ sym.name
      putStrLn $ "  Module: " ++ show sym.symModule
      putStrLn $ "  Raw Unit ID: " ++ show sym.rawUnitId
      putStrLn ""
