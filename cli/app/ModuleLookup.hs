module ModuleLookup
  ( findModuleFile,
    moduleNameToPath,
  )
where

import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | Convert a module name to a relative file path
-- Examples:
--   "Network.HTTP.Simple" → "Network/HTTP/Simple.hs"
--   "Data.Map" → "Data/Map.hs"
moduleNameToPath :: Text -> FilePath
moduleNameToPath moduleName =
  let parts = T.splitOn "." moduleName
      pathParts = T.intercalate "/" parts
   in T.unpack (pathParts <> ".hs")

-- | Common source directory patterns to try
commonSourceDirs :: [FilePath]
commonSourceDirs =
  [ "src",
    "lib",
    "library",
    "source",
    "app",
    "" -- root directory
  ]

-- | Find a module file within a package directory
-- Tries common source directory patterns
-- Returns Nothing if the file is not found
findModuleFile :: FilePath -> Text -> IO (Maybe FilePath)
findModuleFile packageDir moduleName = do
  let relativePath = moduleNameToPath moduleName
  let candidates = map (\dir -> packageDir </> dir </> relativePath) commonSourceDirs

  -- Try all candidates
  findFirstExisting candidates
  where
    -- Find the first existing file from a list of paths
    findFirstExisting :: [FilePath] -> IO (Maybe FilePath)
    findFirstExisting paths = do
      checks <- mapM (\path -> do exists <- doesFileExist path; pure (path, exists)) paths
      pure $ fst <$> find snd checks
