module PackageDownload
  ( downloadPackage,
    cacheDir,
    downloadGitSource
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getHomeDirectory)
import System.FilePath ((</>))
import Data.Text (Text)
import qualified Data.Text as T
import Util (putErr)
import GHC.IO.Exception (ExitCode(..))
import System.Process
import Data.Function ((&))

-- | Get the cache directory for downloaded packages
-- Creates ~/.hls-lookup/haskell-sources/ if it doesn't exist
cacheDir :: IO FilePath
cacheDir = do
  home <- getHomeDirectory
  let cache = home </> ".hls-lookup" </> "haskell-sources"
  createDirectoryIfMissing True cache
  pure cache

-- | Construct Hackage download URL for a package
hackageUrl :: Text -> Text -> Text
hackageUrl name version =
  "https://hackage.haskell.org/package/"
    <> name <> "-" <> version <> "/"
    <> name <> "-" <> version <> ".tar.gz"

-- | Download and extract a package from Hackage
-- Returns the path to the extracted package directory
-- If the package is already cached, skips download and returns the cached path
downloadPackage :: Text -> Text -> IO (Either Text FilePath)
downloadPackage name version = do
  cache <- cacheDir
  let targetDir = T.pack $ cache </> (T.unpack name ++ "-" ++ T.unpack version)

  exists <- doesDirectoryExist $ T.unpack targetDir
  if exists
    then do
      putErr $ "Already cached: " <> targetDir
    else do
      putErr $ "Downloading " <> name <> "-" <> version <> "..."

      -- Download
      request <- parseRequest $ T.unpack $ hackageUrl name version
      response <- httpLBS request
      let tarball = getResponseBody response

      -- Extract
      putErr $ "Extracting to " <> T.pack cache <> "..."
      let entries = Tar.read . GZip.decompress $ tarball
      Tar.unpack cache entries

      putErr $ "Cached: " <> targetDir

  pure $ Right $ T.unpack targetDir


downloadGitSource :: Text -> Text -> IO (Either Text FilePath)
downloadGitSource repoUrl commitHash = do
  cDir <- cacheDir
  let repoPart = repoUrl 
                  & T.replace "https://" "" 
                  & T.replace "/" "-"
      cacheKey = repoPart <> commitHash  -- or just use commitHash
      target = cDir </> T.unpack cacheKey

  -- Check cache first
  cached <- doesDirectoryExist target
  if cached
    then pure $ Right target
    else cloneAndCache repoUrl commitHash target

cloneAndCache :: Text -> Text -> FilePath -> IO (Either Text FilePath)
cloneAndCache url commit dir = do
  createDirectoryIfMissing True dir
  -- Shallow clone for speed
  let path = T.pack dir
      cmd = T.unwords
        [ "git clone --depth 1"
        , url
        , path
        , "&&"
        , "cd", path
        , "&&"
        , "git fetch --depth 1 origin", commit
        , "&&"
        , "git checkout", commit
        ]
  exitCode <- system (T.unpack cmd)
  case exitCode of
    ExitSuccess -> pure $ Right dir
    ExitFailure _ -> pure $ Left ("GitCloneFailed " <> url <> ", " <> commit)