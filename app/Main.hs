module Main where

import qualified HieReader as HR
import PackageDownload (downloadPackage)
import System.Environment (getArgs)
import ModuleLookup (findModuleFile)

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
                    
        ["test-module-lookup", packageName, version, moduleName] -> do
            packageDir <- downloadPackage packageName version
            putStrLn $ "Looking for module: " ++ moduleName
            mFilePath <- findModuleFile packageDir moduleName
            case mFilePath of
                Nothing -> putStrLn $ "Module not found: " ++ moduleName
                Just filePath -> putStrLn $ "Found module at: " ++ filePath

        _ -> putStrLn $ "Usage: hls-lookup download <package> <version>\n       hls-lookup inspect-hie <source-file>\n       "
                        <> "hls-lookup find-symbol <source-file> <line> <col>\n     hls-lookup test-module-lookup <package> <version> <module>"
