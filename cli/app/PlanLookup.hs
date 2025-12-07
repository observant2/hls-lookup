module PlanLookup (shouldDownloadPackage, CabalPlanInfo(..)) where

import Cabal.Plan (PlanJson, PkgId(..), PkgName(..), Unit, UnitId(..), decodePlanJson, pjUnits, uPId, uType, UnitType(..), uId, Ver (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.FilePath ((</>))
import System.Directory (doesFileExist, getCurrentDirectory)
import Control.Exception (catch, SomeException)
import Data.Function ((&))
import Data.List (intercalate)

data CabalPlanInfo =
  ShouldDownload String String -- packageName, packageVersion
  | InternalPackage -- internal package, already handled by hls
  | UseHie -- fallback to raw unitid parsing, TODO: maybe just remove this

-- | Determine if we should download a package based on its Unit ID.
shouldDownloadPackage :: String -> IO CabalPlanInfo
shouldDownloadPackage unitIdStr = do
  mPlan <- loadPlanJson
  case mPlan of
    Nothing -> pure UseHie  -- Plan not found, safe default: assume external (download)
    Just plan -> pure $ checkUnit plan unitIdStr

-- | Load plan.json from dist-newstyle/cache directory
loadPlanJson :: IO (Maybe PlanJson)
loadPlanJson = do
  cwd <- getCurrentDirectory
  let planPath = cwd </> "dist-newstyle" </> "cache" </> "plan.json"
  exists <- doesFileExist planPath
  if not exists
    then pure Nothing
    else catch (Just <$> decodePlanJson planPath)
               (\(_ :: SomeException) -> pure Nothing)

-- | Check if a unit should be downloaded by looking it up in the plan
checkUnit :: PlanJson -> String -> CabalPlanInfo
checkUnit plan unitIdStr =
  let units = pjUnits plan
   in case findUnit units unitIdStr of
        Nothing -> UseHie  -- Unit not found in plan
        Just unit -> shouldDownload unit

-- | Determine if a specific unit should be downloaded
shouldDownload :: Unit -> CabalPlanInfo
shouldDownload u =
  if not (isLocalPackage u || isBootLibrary u) then
    extractNameAndVersion u
  else
    InternalPackage

extractNameAndVersion :: Unit -> CabalPlanInfo
extractNameAndVersion u = case u.uPId of
    (PkgId (PkgName name) version) -> ShouldDownload (T.unpack name) (toVersionString version)

toVersionString :: Ver -> String
toVersionString (Ver version) = version & map show & intercalate "."

-- | Check if a unit is a local package (our own code, not from Hackage)
isLocalPackage :: Unit -> Bool
isLocalPackage u = case uType u of
  UnitTypeLocal    -> True  -- Local package in project
  UnitTypeInplace  -> True  -- Local in-place package
  UnitTypeBuiltin  -> False -- Pre-existing (may be downloadable, check isBootLibrary separately)
  UnitTypeGlobal   -> False -- External Hackage package

-- | Check if a unit is a GHC boot library (base, ghc-prim, etc.)
-- Boot libraries are handled by HLS and should not be downloaded
isBootLibrary :: Unit -> Bool
isBootLibrary u =
  let PkgId (PkgName pkgName) _version = uPId u
      name = T.unpack pkgName
      bootLibPrefixes = ["ghc-", "base-", "ghc-prim-", "ghc-bignum-", "ghc-internal-"]
   in any (`isPrefixOf` name) bootLibPrefixes
  where
    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- | Find a unit in the plan by its Unit ID string
findUnit :: Map.Map k Unit -> String -> Maybe Unit
findUnit units searchId =
  let searchText = T.pack searchId
   in Map.foldl' (\acc u ->
        case uId u of
          UnitId unitText -> if unitText == searchText then Just u else acc)
      Nothing units
