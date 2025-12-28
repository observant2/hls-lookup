module SymbolLookup
  ( getSymbolsAtPosition,
    containsPosition,
  )
where

import Data.Map qualified as Map
import GHC.Iface.Ext.Types
import GHC.Types.Name (Name, nameModule_maybe, nameOccName)
import qualified Data.Text as T
import GHC.Types.SrcLoc (RealSrcSpan, realSrcSpanEnd, realSrcSpanStart, srcLocCol, srcLocLine)
import GHC.Unit.Module (moduleName, moduleNameString, moduleUnit)
import GHC.Unit.Types (toUnitId, unitIdString)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import LookupTypes (SymbolInfo (..))

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

-- | Convert Name to SymbolInfo
nameToSymbolInfo :: RealSrcSpan -> Name -> SymbolInfo
nameToSymbolInfo span name =
  let modMaybe = nameModule_maybe name
      rawUnit = case modMaybe of
        Nothing -> Nothing
        Just mod ->
          let unit = mod.moduleUnit
              unitId = toUnitId unit
              unitStr = unitIdString unitId
           in Just unitStr
   in SymbolInfo
        { name = T.pack $ showSDocUnsafe (ppr $ nameOccName name),
          symModule = T.pack . moduleString <$> modMaybe,
          rawUnitId = T.pack <$> rawUnit,
          span = span
        }
  where
    moduleString = moduleNameString . moduleName

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

-- | Keep only symbols from the most specific (last/deepest) span
-- The AST traversal is depth-first with parents before children,
-- so the last symbols are from the innermost/most specific node
filterSmallestSpan :: [SymbolInfo] -> [SymbolInfo]
filterSmallestSpan [] = []
filterSmallestSpan symbols =
  let lastSpan = (last symbols).span
   in filter (\sym -> sym.span == lastSpan) symbols

-- | Get all symbols at a specific line and column
-- Returns symbols from the most specific (innermost) span only
getSymbolsAtPosition :: HieFile -> Int -> Int -> [SymbolInfo]
getSymbolsAtPosition hieFile line col =
  let asts = getAsts hieFile.hie_asts
      allSymbols = concatMap (findSymbolsInAST line col) $ Map.elems asts
      -- Keep only symbols with the smallest (most specific) span
      smallestSymbols = filterSmallestSpan allSymbols
   in smallestSymbols
