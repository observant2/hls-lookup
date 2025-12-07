module PackageDownload
  ( downloadPackage,
    cacheDir,
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getHomeDirectory)
import System.FilePath ((</>))

-- | Get the cache directory for downloaded packages
-- Creates ~/.hls-lookup/haskell-sources/ if it doesn't exist
cacheDir :: IO FilePath
cacheDir = do
  home <- getHomeDirectory
  let cache = home </> ".hls-lookup" </> "haskell-sources"
  createDirectoryIfMissing True cache
  pure cache

-- | Construct Hackage download URL for a package
hackageUrl :: String -> String -> String
hackageUrl name version =
  "https://hackage.haskell.org/package/"
    ++ name ++ "-" ++ version ++ "/"
    ++ name ++ "-" ++ version ++ ".tar.gz"

-- | Download and extract a package from Hackage
-- Returns the path to the extracted package directory
-- If the package is already cached, skips download and returns the cached path
downloadPackage :: String -> String -> IO FilePath
downloadPackage name version = do
  cache <- cacheDir
  let targetDir = cache </> (name ++ "-" ++ version)

  exists <- doesDirectoryExist targetDir
  if exists
    then do
      putStrLn $ "Already cached: " ++ targetDir
    else do
      putStrLn $ "Downloading " ++ name ++ "-" ++ version ++ "..."

      -- Download
      request <- parseRequest $ hackageUrl name version
      response <- httpLBS request
      let tarball = getResponseBody response

      -- Extract
      putStrLn $ "Extracting to " ++ cache ++ "..."
      let entries = Tar.read . GZip.decompress $ tarball
      Tar.unpack cache entries

      putStrLn $ "Cached: " ++ targetDir

  pure targetDir
