module Main where

import qualified HieReader as HR
import PackageDownload (downloadPackage)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["download", name, version] -> do
            path <- downloadPackage name version
            putStrLn $ "Source location: " ++ path
            
        ["inspect-hie", srcFile] -> do
            mHiePath <- HR.findHieFile srcFile
            case mHiePath of
                Nothing -> putStrLn $ "No .hie file found for " ++ srcFile
                Just hiePath -> do
                    putStrLn $ "Found .hie file: " ++ hiePath
                    HR.inspectHieFile hiePath
        
        ["find-symbol", srcFile, lineStr, colStr] -> do
            let line = read lineStr :: Int
            let col = read colStr :: Int
            mHiePath <- HR.findHieFile srcFile
            case mHiePath of
                Nothing -> putStrLn $ "No .hie file found for " ++ srcFile
                Just hiePath -> HR.findSymbolAt hiePath line col
                    
        _ -> putStrLn "Usage: hls-lookup download <package> <version>\n       hls-lookup inspect-hie <source-file>"
