module HieReader where

import Data.Char
import Data.List qualified as L (break, reverse, span)
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

-- | name, symModule, PackageVersion of a hackage package
data SymbolInfo = SymbolInfo
  { name :: String,
    symModule :: Maybe String,
    packageName :: Maybe String,
    packageVersion :: Maybe String,
    rawUnitId :: Maybe String, -- Debug: raw unit ID string
    span :: RealSrcSpan
  }
  deriving (Show)

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

-- | Check if a position is within a span (span line col)
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

-- | Parse package name and version from UnitId string
--  Format: "pkgname-1.2.3.4" or "pkgname-1.2.3.4-hash" or "pkgname-1.2.3.4:libname+hash"
parseUnitId :: String -> (Maybe String, Maybe String)
parseUnitId unitStr =
  let -- Remove any ":libname+hash" suffix
      baseStr = case L.break (== ':') unitStr of
        (base, _) -> base
      -- Split on '-' and work backwards to find version
      parts = splitOn '-' baseStr
      -- Remove hash from end if present (long hex string, typically 64 chars)
      partsNoHash = case L.reverse parts of
        [] -> []
        (lastPart : rest) ->
          if isHash lastPart
            then L.reverse rest
            else L.reverse parts
   in extractNameVersion (L.reverse partsNoHash)
  where
    -- Split string on a character
    splitOn :: Char -> String -> [String]
    splitOn _ "" = []
    splitOn c s = case L.break (== c) s of
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
      let (versionParts, nameParts) = L.span isVersionPart parts
       in case (nameParts, versionParts) of
            ([], _) -> (Nothing, Nothing) -- No package name
            (_, []) -> (Just $ concatWith "-" (L.reverse nameParts), Nothing) -- No version
            _ ->
              ( Just $ concatWith "-" (L.reverse nameParts),
                Just $ concatWith "." (L.reverse versionParts)
              )

    -- Check if a string part looks like a version component (digits and dots)
    isVersionPart :: String -> Bool
    isVersionPart s = not (null s) && all (\c -> isDigit c || c == '.') s && any isDigit s

    -- Concatenate with specified separator
    concatWith :: String -> [String] -> String
    concatWith _ [] = ""
    concatWith _ [x] = x
    concatWith sep (x : xs) = x ++ sep ++ concatWith sep xs

-- | Convert Name to SymbolInfo
nameToSymbolInfo :: RealSrcSpan -> Name -> SymbolInfo
nameToSymbolInfo span name =
  let modMaybe = nameModule_maybe name
      (pkgName, pkgVersion, rawUnit) = case modMaybe of
        Nothing -> (Nothing, Nothing, Nothing)
        Just mod ->
          let unit = mod.moduleUnit
              unitId = toUnitId unit
              unitStr = unitIdString unitId -- TODO: Check here if it is ghc-, then no need to jump to source
              (name', ver') = parseUnitId unitStr
           in (name', ver', Just unitStr)
   in SymbolInfo
        { name = showSDocUnsafe (ppr $ nameOccName name),
          symModule = moduleString <$> modMaybe,
          packageName = pkgName,
          packageVersion = pkgVersion,
          rawUnitId = rawUnit,
          span = span
        }

-- | Traverse AST and collect all symbols at position
findSymbolsInAST :: Int -> Int -> HieAST a -> [SymbolInfo]
findSymbolsInAST line col (Node info span children) =
  let currentSymbols =
        if containsPosition span line col
          then extractSymbols span info
          else []
      childSymbols = concatMap (findSymbolsInAST line col) children
   in currentSymbols ++ childSymbols

-- | Extract symbols from NodeInfo
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

-- | Pretty print a Module
moduleString :: Module -> String
moduleString = moduleNameString . moduleName

-- Test function to inspect what's in a .hie file
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
