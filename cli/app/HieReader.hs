module HieReader
  ( -- * Types
    SymbolInfo (..),

    -- * Loading HIE files
    loadHieFile,
    findHieFile,

    -- * Symbol lookup
    getSymbolsAtPosition,
    containsPosition,
    isBootLibrary,

    -- * Parsing utilities
    parseUnitId,

    -- * Testing and inspection
    inspectHieFile,
    findSymbolAt,
  )
where

import Data.Map qualified as Map
import GHC.Iface.Ext.Binary (HieFileResult (..), readHieFile)
import GHC.Iface.Ext.Types (HieFile (hie_asts, hie_hs_file, hie_module), getAsts)
import GHC.Types.Name.Cache (initNameCache)
import GHC.Unit.Module (moduleName, moduleNameString)
import GHC.Unit.Types (Module)
import HieReader.Parser (parseUnitId)
import HieReader.SymbolLookup (containsPosition, getSymbolsAtPosition, isBootLibrary)
import HieReader.Types (SymbolInfo (..))
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName, (<.>), (</>))

-- | Load a .hie file from FilePath
loadHieFile :: FilePath -> IO HieFile
loadHieFile hiePath = do
  nameCache <- initNameCache 'z' []
  result <- readHieFile nameCache hiePath
  return result.hie_file_result

-- | Find .hie file for a source file
findHieFile :: FilePath -> IO (Maybe FilePath)
findHieFile srcFile = do
  let baseName = takeBaseName srcFile
  let hiePath = ".hie" </> baseName <.> "hie"
  exists <- doesFileExist hiePath
  return $ if exists then Just hiePath else Nothing

-- | Pretty print a Module
moduleString :: Module -> String
moduleString = moduleNameString . moduleName

-- | Test function to inspect what's in a .hie file
inspectHieFile :: FilePath -> IO ()
inspectHieFile hiePath = do
  hieFile <- loadHieFile hiePath
  putStrLn $ "Module: " ++ moduleString hieFile.hie_module
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
      putStrLn $ "  Package Name: " ++ show sym.packageName
      putStrLn $ "  Package Version: " ++ show sym.packageVersion

      -- Debug: show what the combined parsing resulted in
      case (sym.packageName, sym.packageVersion) of
        (Just name, Just ver) ->
          putStrLn $ "  -> Would download: " ++ name ++ " version " ++ ver
        _ ->
          pure ()
      putStrLn ""
