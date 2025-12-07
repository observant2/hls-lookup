module Main where

import qualified HieReader as HR
import PackageDownload (downloadPackage)
import System.Environment (getArgs)
import ModuleLookup (findModuleFile)
import DefinitionFinder (findDefinition, DefinitionLocation(..))
import System.Process (callCommand)
import System.Exit (exitFailure)
import Data.Maybe

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

        ["goto", srcFile, lineStr, colStr] -> gotoDefinition srcFile (read lineStr) (read colStr) True

        ["goto-print", srcFile, lineStr, colStr] -> gotoDefinition srcFile (read lineStr) (read colStr) False

        _ -> putStrLn "Usage:\n\
                        \  hls-lookup download <package> <version>\n\
                        \  hls-lookup inspect-hie <source-file>\n\
                        \  hls-lookup find-symbol <source-file> <line> <col>\n\
                        \  hls-lookup test-module-lookup <package> <version> <module>\n\
                        \  hls-lookup goto <source-file> <line> <col>        - Find and open definition in VSCode\n\
                        \  hls-lookup goto-print <source-file> <line> <col>  - Find and print definition location"

-- | Main goto definition workflow
gotoDefinition :: FilePath -> Int -> Int -> Bool -> IO ()
gotoDefinition srcFile line col shouldOpen = do
    -- Step 1: Find the .hie file
    putStrLn $ "Looking for HIE file for: " ++ srcFile
    mHiePath <- HR.findHieFile srcFile
    case mHiePath of
        Nothing -> do
            putStrLn $ "Error: No .hie file found for " ++ srcFile
            putStrLn "Hint: Make sure the project is compiled with -fwrite-ide-info"
            exitFailure
        Just hiePath -> do
            putStrLn $ "Found HIE file: " ++ hiePath

            -- Step 2: Load HIE file and get symbols at position
            putStrLn $ "Finding symbol at " ++ show line ++ ":" ++ show col
            hieFile <- HR.loadHieFile hiePath
            let symbols = HR.getSymbolsAtPosition hieFile line col
            case reverse symbols of
                [] -> do
                    putStrLn "Error: No symbol found at the specified position"
                    exitFailure
                (sym:_) -> do
                    putStrLn $ "Found symbol: " ++ sym.name
                    putStrLn $ "  Module: " ++ fromMaybe "<unknown>" sym.symModule
                    case (sym.symModule, sym.packageName, sym.packageVersion) of
                        (Just modName, Just pkgName, Just pkgVer) -> do
                            putStrLn $ "  Package: " ++ pkgName ++ "-" ++ pkgVer

                            -- Step 3: Download the package
                            putStrLn "\nDownloading package..."
                            packageDir <- downloadPackage pkgName pkgVer
                            putStrLn $ "Package cached at: " ++ packageDir

                            continueWithPackage packageDir modName sym.name shouldOpen
                        _ -> do
                            putStrLn "Error: Could not determine package information"
                            putStrLn $ "  Raw Unit ID: " ++ fromMaybe "<none>" sym.rawUnitId
                            exitFailure

-- | Continue the goto workflow with the downloaded package
continueWithPackage :: FilePath -> String -> String -> Bool -> IO ()
continueWithPackage packageDir moduleName symbolName shouldOpen = do
    -- Step 4: Find the module file
    putStrLn $ "\nLooking for module: " ++ moduleName
    mModuleFile <- findModuleFile packageDir moduleName
    case mModuleFile of
        Nothing -> do
            putStrLn $ "Error: Could not find module file for " ++ moduleName
            exitFailure
        Just moduleFile -> do
            putStrLn $ "Found module at: " ++ moduleFile

            -- Step 5: Find the definition in the file
            putStrLn $ "\nSearching for definition of: " ++ symbolName
            mDefLoc <- findDefinition moduleFile symbolName
            case mDefLoc of
                Nothing -> do
                    putStrLn $ "Warning: Could not find definition of " ++ symbolName ++ " in " ++ moduleFile
                    putStrLn "Opening file at top..."
                    if shouldOpen
                        then openInVSCode moduleFile 1 1
                        else printLocation moduleFile 1 1
                Just defLoc -> do
                    putStrLn $ "Found definition at line " ++ show defLoc.line ++ ", column " ++ show defLoc.column
                    if shouldOpen
                        then openInVSCode moduleFile defLoc.line defLoc.column
                        else printLocation moduleFile defLoc.line defLoc.column

-- | Open a file in VSCode at a specific line and column
openInVSCode :: FilePath -> Int -> Int -> IO ()
openInVSCode filePath line col = do
    let location = filePath ++ ":" ++ show line ++ ":" ++ show col
    putStrLn $ "\nOpening in VSCode: " ++ location
    callCommand $ "code --goto " ++ show location

-- | Print the file location (for scripting/integration)
printLocation :: FilePath -> Int -> Int -> IO ()
printLocation filePath line col = do
    putStrLn "\n=== DEFINITION LOCATION ==="
    putStrLn $ filePath ++ ":" ++ show line ++ ":" ++ show col
