{-# LANGUAGE OverloadedStrings #-}

module HieReader where

import Data.Char
import Data.List (break, reverse, span)
import Data.Map qualified as Map
import GHC.Iface.Ext.Binary (HieFileResult (..), readHieFile)
import GHC.Iface.Ext.Types
import GHC.Types.Name (Name, nameModule_maybe, nameOccName)
import GHC.Types.Name.Cache (initNameCache)
import GHC.Types.SrcLoc (RealSrcSpan, realSrcSpanEnd, realSrcSpanStart, srcLocCol, srcLocLine)
import GHC.Unit.Module (moduleName, moduleNameString, moduleUnit)
import GHC.Unit.Types (Module, toUnitId, unitIdString)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import System.Directory
import System.FilePath

-- Load a .hie file
loadHieFile :: FilePath -> IO HieFile
loadHieFile hiePath = do
  nameCache <- initNameCache 'z' []
  result <- readHieFile nameCache hiePath
  return result.hie_file_result

-- Find .hie file for a source file
findHieFile :: FilePath -> IO (Maybe FilePath)
findHieFile srcFile = do
  let baseName = takeBaseName srcFile
  let hiePath = ".hie" </> baseName <.> "hie"
  exists <- doesFileExist hiePath
  return $ if exists then Just hiePath else Nothing

-- Check if a position is within a span
containsPosition :: RealSrcSpan -> Int -> Int -> Bool
containsPosition span line col =
  let start = realSrcSpanStart span
      end = realSrcSpanEnd span
      startLine = srcLocLine start
      startCol = srcLocCol start
      endLine = srcLocLine end
      endCol = srcLocCol end
   in (startLine < line || (startLine == line && startCol <= col))
        && (endLine > line || (endLine == line && endCol >= col))

-- Extract identifier information with full Name
data SymbolInfo = SymbolInfo
  { symName :: String,
    symModule :: Maybe String,
    symPackageName :: Maybe String,
    symPackageVersion :: Maybe String,
    symRawUnitId :: Maybe String, -- Debug: raw unit ID string
    symSpan :: RealSrcSpan
  }
  deriving (Show)

-- Parse package name and version from UnitId string
-- Format: "pkgname-1.2.3.4" or "pkgname-1.2.3.4-hash" or "pkgname-1.2.3.4:libname+hash"
parseUnitId :: String -> (Maybe String, Maybe String)
parseUnitId unitStr =
  let -- Remove any ":libname+hash" suffix
      baseStr = case Data.List.break (== ':') unitStr of
        (base, _) -> base
      -- Split on '-' and work backwards to find version
      parts = splitOn '-' baseStr
      -- Remove hash from end if present (long hex string, typically 64 chars)
      partsNoHash = case Data.List.reverse parts of
        [] -> []
        (lastPart : rest) ->
          if isHash lastPart
            then Data.List.reverse rest
            else Data.List.reverse parts
   in extractNameVersion (Data.List.reverse partsNoHash)
  where
    -- Split string on a character
    splitOn :: Char -> String -> [String]
    splitOn _ "" = []
    splitOn c s = case Data.List.break (== c) s of
      (chunk, "") -> [chunk]
      (chunk, _ : rest) -> chunk : splitOn c rest

    -- Check if a string looks like a hash (long hex string)
    isHash :: String -> Bool
    isHash s = length s > 32 && all isHexChar s
      where
        isHexChar c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

    -- Extract version parts from the end (already reversed)
    extractNameVersion :: [String] -> (Maybe String, Maybe String)
    extractNameVersion [] = (Nothing, Nothing)
    extractNameVersion parts =
      let (versionParts, nameParts) = Data.List.span isVersionPart parts
       in case (nameParts, versionParts) of
            ([], _) -> (Nothing, Nothing) -- No package name
            (_, []) -> (Just $ concatWith "-" (Data.List.reverse nameParts), Nothing) -- No version
            _ ->
              ( Just $ concatWith "-" (Data.List.reverse nameParts),
                Just $ concatWith "." (Data.List.reverse versionParts)
              )

    -- Check if a string part looks like a version component (digits and dots)
    isVersionPart :: String -> Bool
    isVersionPart s = not (null s) && all (\c -> isDigit c || c == '.') s && any isDigit s

    -- Concatenate with specified separator
    concatWith :: String -> [String] -> String
    concatWith _ [] = ""
    concatWith _ [x] = x
    concatWith sep (x : xs) = x ++ sep ++ concatWith sep xs

-- Convert Name to SymbolInfo
nameToSymbolInfo :: RealSrcSpan -> Name -> SymbolInfo
nameToSymbolInfo span name =
  let modMaybe = nameModule_maybe name
      (pkgName, pkgVersion, rawUnit) = case modMaybe of
        Nothing -> (Nothing, Nothing, Nothing)
        Just mod ->
          let unit = moduleUnit mod
              unitId = toUnitId unit
              unitStr = unitIdString unitId
              (name', ver') = parseUnitId unitStr
           in (name', ver', Just unitStr)
   in SymbolInfo
        { symName = showSDocUnsafe (ppr $ nameOccName name),
          symModule = fmap moduleString modMaybe,
          symPackageName = pkgName,
          symPackageVersion = pkgVersion,
          symRawUnitId = rawUnit,
          symSpan = span
        }

-- Traverse AST and collect all symbols at position
findSymbolsInAST :: Int -> Int -> HieAST a -> [SymbolInfo]
findSymbolsInAST line col (Node info span children) =
  let currentSymbols =
        if containsPosition span line col
          then extractSymbols span info
          else []
      childSymbols = concatMap (findSymbolsInAST line col) children
   in currentSymbols ++ childSymbols

-- Extract symbols from NodeInfo
extractSymbols :: RealSrcSpan -> SourcedNodeInfo a -> [SymbolInfo]
extractSymbols span (SourcedNodeInfo nodeInfos) =
  concatMap (extractFromNodeInfo span) $ Map.elems nodeInfos

extractFromNodeInfo :: RealSrcSpan -> NodeInfo a -> [SymbolInfo]
extractFromNodeInfo span (NodeInfo _ _ identMap) =
  [nameToSymbolInfo span name | (Right name, _) <- Map.toList identMap]

-- Get all symbols at a specific line and column
getSymbolsAtPosition :: HieFile -> Int -> Int -> [SymbolInfo]
getSymbolsAtPosition hieFile line col =
  let asts = getAsts hieFile.hie_asts
      allSymbols = concatMap (findSymbolsInAST line col) $ Map.elems asts
   in allSymbols

-- Pretty print a Module
moduleString :: Module -> String
moduleString = moduleNameString . moduleName

-- Test function to inspect what's in a .hie file
inspectHieFile :: FilePath -> IO ()
inspectHieFile hiePath = do
  hieFile <- loadHieFile hiePath
  putStrLn $ "Module: " ++ moduleString hieFile.hie_module
  putStrLn $ "Source file: " ++ hieFile.hie_hs_file
  putStrLn $ "AST entries: " ++ show (Map.size $ getAsts hieFile.hie_asts)

-- Test function to find symbols at a position
findSymbolAt :: FilePath -> Int -> Int -> IO ()
findSymbolAt hiePath line col = do
  hieFile <- loadHieFile hiePath
  let symbols = getSymbolsAtPosition hieFile line col
  putStrLn $ "Symbols at " ++ show line ++ ":" ++ show col ++ ":"
  mapM_ printSymbol symbols
  where
    printSymbol sym = do
      putStrLn $ "  Name: " ++ sym.symName
      putStrLn $ "  Module: " ++ show sym.symModule
      putStrLn $ "  Raw Unit ID: " ++ show sym.symRawUnitId
      putStrLn $ "  Package Name: " ++ show sym.symPackageName
      putStrLn $ "  Package Version: " ++ show sym.symPackageVersion
      -- Debug: show what the combined parsing resulted in
      case (sym.symPackageName, sym.symPackageVersion) of
        (Just name, Just ver) -> putStrLn $ "  -> Would download: " ++ name ++ " version " ++ ver
        _ -> return ()
      putStrLn ""
