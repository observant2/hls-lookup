module Main where
import System.Directory (getHomeDirectory, createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import Network.HTTP.Simple
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import System.Environment
import HieReader

cacheDir :: IO FilePath
cacheDir = do
    home <- getHomeDirectory
    let cache = home </> ".hls-lookup" </> "haskell-sources"
    createDirectoryIfMissing True cache
    pure cache

hackageUrl :: String -> String -> String
hackageUrl name version =
    "https://hackage.haskell.org/package/"
    ++ name ++ "-" ++ version
    ++ "/" ++ name ++ "-" ++ version ++ ".tar.gz"

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

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["download", name, version] -> do
            path <- downloadPackage name version
            putStrLn $ "Source location: " ++ path
            
        ["inspect-hie", srcFile] -> do
            mHiePath <- findHieFile srcFile
            case mHiePath of
                Nothing -> putStrLn $ "No .hie file found for " ++ srcFile
                Just hiePath -> do
                    putStrLn $ "Found .hie file: " ++ hiePath
                    inspectHieFile hiePath
        
        ["find-symbol", srcFile, lineStr, colStr] -> do
            let line = read lineStr :: Int
            let col = read colStr :: Int
            mHiePath <- findHieFile srcFile
            case mHiePath of
                Nothing -> putStrLn $ "No .hie file found for " ++ srcFile
                Just hiePath -> findSymbolAt hiePath line col
                    
        _ -> putStrLn "Usage: hls-lookup download <package> <version>\n       hls-lookup inspect-hie <source-file>"
