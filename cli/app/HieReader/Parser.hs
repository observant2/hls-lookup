module HieReader.Parser
  ( parseUnitId,
  )
where

import Data.Char (isDigit)
import Data.List qualified as L (break, reverse, span)
import Util (splitOn)
import Data.List (intercalate)

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
            else parts
   in extractNameVersion partsNoHash

-- | Check if a character is a hexadecimal digit
isHexChar :: Char -> Bool
isHexChar c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

-- | Check if a string looks like a hash (long hex string)
isHash :: String -> Bool
isHash s = length s > 32 && all isHexChar s

-- | Check if a string looks like a short hash (all hex, but <= 32 chars)
isShortHash :: String -> Bool
isShortHash s = not (null s) && length s <= 32 && all isHexChar s

-- | Extract version parts from the end, working backwards
extractNameVersion :: [String] -> (Maybe String, Maybe String)
extractNameVersion [] = (Nothing, Nothing)
extractNameVersion parts =
  let -- Reverse to work from the end
      reversedParts = L.reverse parts
      -- Remove potential short hash from the end first (e.g., "abc123")
      partsAfterHash = case reversedParts of
        (h : rest) | isShortHash h -> rest
        _ -> reversedParts
      -- Take version parts from the end
      (versionParts, nameParts) = L.span isVersionPart partsAfterHash
   in case (nameParts, versionParts) of
        ([], _) -> (Nothing, Nothing) -- No package name
        (_, []) -> (Just $ intercalate "-" (L.reverse nameParts), Nothing) -- No version
        _ ->
          ( Just $ intercalate "-" (L.reverse nameParts),
            Just $ intercalate "." (L.reverse versionParts)
          )

-- | Check if a string part looks like a version component (digits and dots)
isVersionPart :: String -> Bool
isVersionPart s = not (null s) && all (\c -> isDigit c || c == '.') s && any isDigit s
