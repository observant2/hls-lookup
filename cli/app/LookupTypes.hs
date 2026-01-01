module LookupTypes(SymbolInfo(..)) where

import GHC.Types.SrcLoc (RealSrcSpan)
import Data.Text (Text)

-- | Information about a symbol found at a specific location
-- Contains the symbol name, its module, package information, and source span
data SymbolInfo = SymbolInfo
  { name :: Text,
    symModule :: Maybe Text,
    rawUnitId :: Maybe Text,
    span :: RealSrcSpan
  }
  deriving (Show)
