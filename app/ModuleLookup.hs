module ModuleLookup
  ( findModuleFile,
    moduleNameToPath,
  )
where

import Data.List (find, intercalate)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | Convert a module name to a relative file path
-- Examples:
--   "Network.HTTP.Simple" → "Network/HTTP/Simple.hs"
--   "Data.Map" → "Data/Map.hs"
moduleNameToPath :: String -> FilePath
moduleNameToPath moduleName =
  let parts = splitOn '.' moduleName
      pathParts = intercalate "/" parts
   in pathParts ++ ".hs"
  where
    -- Split string on a character
    splitOn :: Char -> String -> [String]
    splitOn _ "" = [""]
    splitOn c s = case break (== c) s of
      (chunk, "") -> [chunk]
      (chunk, _ : rest) -> chunk : splitOn c rest

-- | Common source directory patterns to try
commonSourceDirs :: [FilePath]
commonSourceDirs =
  [ "src",
    "lib",
    "source",
    "app",
    "" -- root directory
  ]

-- | Find a module file within a package directory
-- Tries common source directory patterns
-- Returns Nothing if the file is not found
findModuleFile :: FilePath -> String -> IO (Maybe FilePath)
findModuleFile packageDir moduleName = do
  let relativePath = moduleNameToPath moduleName
  let candidates = map (\dir -> packageDir </> dir </> relativePath) commonSourceDirs

  -- Also try .lhs (literate Haskell) extension
  let lhsCandidates = map (\path -> take (length path - 3) path ++ ".lhs") candidates

  -- Try all candidates
  findFirstExisting (candidates ++ lhsCandidates)
  where
    -- Find the first existing file from a list of paths
    findFirstExisting :: [FilePath] -> IO (Maybe FilePath)
    findFirstExisting paths = do
      checks <- mapM (\path -> do exists <- doesFileExist path; return (path, exists)) paths
      pure $ fst <$> find snd checks
