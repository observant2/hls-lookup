module LookupTypes(SymbolInfo(..)) where

import GHC.Types.SrcLoc (RealSrcSpan)

-- | Information about a symbol found at a specific location
-- Contains the symbol name, its module, package information, and source span
data SymbolInfo = SymbolInfo
  { name :: String,
    symModule :: Maybe String,
    rawUnitId :: Maybe String,
    span :: RealSrcSpan
  }
  deriving (Show)
