module PlanLookup (shouldDownloadPackage, CabalPlanInfo(..)) where

import Cabal.Plan (PlanJson, PkgId(..), PkgName(..), Unit (uPkgSrc), UnitId(..), decodePlanJson, pjUnits, uPId, uType, UnitType(..), uId, Ver (..), PkgLoc (RemoteSourceRepoPackage), SourceRepo (..), RepoType (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.Directory (doesFileExist, getCurrentDirectory)
import Control.Exception (catch, SomeException)
import Data.Function ((&))

data CabalPlanInfo =
  HackagePackage Text Text -- packageName, packageVersion
  | GitRepo (Text,Text) -- (url,tag)
  | InternalPackage -- internal package, already handled by hls
  | UseHie -- fallback to raw unitid parsing, TODO: maybe just remove this
  deriving (Show)

-- | Determine if we should download a package based on its Unit ID.
shouldDownloadPackage :: Text -> IO CabalPlanInfo
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
checkUnit :: PlanJson -> Text -> CabalPlanInfo
checkUnit plan unitIdStr =
  let units = pjUnits plan
   in case findUnit units unitIdStr of
        Nothing -> UseHie  -- Unit not found in plan
        Just unit -> shouldDownload unit

-- | Determine if a specific unit should be downloaded
shouldDownload :: Unit -> CabalPlanInfo
shouldDownload u
  | isGitRepo u = extractGitRepo u
  | not (isLocalPackage u || isBootLibrary u) = extractNameAndVersion u
  | otherwise = InternalPackage

isGitRepo :: Unit -> Bool
isGitRepo u = case u.uPkgSrc of
  Just (RemoteSourceRepoPackage repo) -> isGitSourceRepo repo
  _ -> False
  where
    isGitSourceRepo repo = case repo.srType of
      Just Git -> True
      _ -> False

extractGitRepo :: Unit -> CabalPlanInfo
extractGitRepo u =
  maybe UseHie GitRepo (extractGitLocation u)
  where
    extractGitLocation :: Unit -> Maybe (Text, Text)
    extractGitLocation unit = do
      src <- unit.uPkgSrc
      RemoteSourceRepoPackage repo <- pure src
      Git <- repo.srType
      location <- repo.srLocation
      tag <- repo.srTag
      pure (location,tag)

extractNameAndVersion :: Unit -> CabalPlanInfo
extractNameAndVersion u = case u.uPId of
    (PkgId (PkgName name) version) -> HackagePackage name (toVersionString version)

toVersionString :: Ver -> Text
toVersionString (Ver version) = version & map T.show & T.intercalate "."

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
findUnit :: Map.Map k Unit -> Text -> Maybe Unit
findUnit units searchId =
  let searchText = searchId
   in Map.foldl' (\acc u ->
        case uId u of
          UnitId unitText -> if unitText == searchText then Just u else acc)
      Nothing units
