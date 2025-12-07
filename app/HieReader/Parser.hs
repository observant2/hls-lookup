module HieReader.Parser
  ( parseUnitId,
  )
where

import Data.Char (isDigit)
import Data.List qualified as L (break, reverse, span)
import Util (splitOn)

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

-- | Check if a string looks like a hash (long hex string)
isHash :: String -> Bool
isHash s = length s > 32 && all isHexChar s
  where
    isHexChar c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

-- | Extract version parts from the end (already reversed)
extractNameVersion :: [String] -> (Maybe String, Maybe String)
extractNameVersion [] = (Nothing, Nothing)
extractNameVersion parts =
  let (versionParts, nameParts) = L.span isVersionPart parts
   in case (nameParts, versionParts) of
        ([], _) -> (Nothing, Nothing) -- No package name
        -- TODO: this part is super confusing with the reverse stuff
        (_, []) -> (Just $ concatWith "-" (L.reverse nameParts), Nothing) -- No version
        _ ->
          ( Just $ concatWith "-" (L.reverse nameParts),
            Just $ concatWith "." (L.reverse versionParts)
          )

-- | Check if a string part looks like a version component (digits and dots)
isVersionPart :: String -> Bool
isVersionPart s = not (null s) && all (\c -> isDigit c || c == '.') s && any isDigit s

-- | Concatenate with specified separator
concatWith :: String -> [String] -> String
concatWith _ [] = ""
concatWith _ [x] = x
concatWith sep (x : xs) = x ++ sep ++ concatWith sep xs
