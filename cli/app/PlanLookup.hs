module PlanLookup (isLocalPackage) where

import Cabal.Plan (PlanJson, decodePlanJson, pjUnits, uType, UnitType(..), uId)
import qualified Data.Map.Strict as Map
import System.FilePath ((</>))
import System.Directory (doesFileExist, getCurrentDirectory)
import Control.Exception (catch, SomeException)

-- | Check if a Unit ID is from a local package
-- Returns: Just True (local), Just False (external), Nothing (can't determine)
isLocalPackage :: String -> IO (Maybe Bool)
isLocalPackage unitIdStr = do
  mPlan <- loadPlanJson
  case mPlan of
    Nothing -> pure Nothing  -- Plan not found, assume external
    Just plan -> pure $ lookupUnitStyle plan unitIdStr

loadPlanJson :: IO (Maybe PlanJson)
loadPlanJson = do
  cwd <- getCurrentDirectory
  let planPath = cwd </> "dist-newstyle" </> "cache" </> "plan.json"
  exists <- doesFileExist planPath
  if not exists
    then pure Nothing
    else catch (Just <$> decodePlanJson planPath)
               (\(_ :: SomeException) -> pure Nothing)

lookupUnitStyle :: PlanJson -> String -> Maybe Bool
lookupUnitStyle plan unitIdStr =
  let units = pjUnits plan
   in case findUnit units unitIdStr of
        Nothing -> Nothing
        Just unit -> Just (isLocal unit)
  where
    isLocal u = case uType u of
      UnitTypeLocal -> True
      UnitTypeInplace -> True
      _ -> False

    findUnit units searchId =
      Map.foldl' (\acc u -> if show (uId u) == searchId then Just u else acc)
                 Nothing units
