module Main where

import qualified HieReader as HR
import PackageDownload (downloadPackage)
import System.Environment (getArgs)
import ModuleLookup (findModuleFile)
import DefinitionFinder (findDefinition, DefinitionLocation(..))
import System.Process (callCommand)
import System.Exit (exitFailure)
import Data.Maybe ( fromMaybe )
import Types (GotoResponse(GotoResponse))
import Data.Aeson as Aeson ( encode )
import qualified Data.ByteString.Lazy.Char8 as BS
import Util (putErr)

data GotoAction 
    = Print 
    | PrintJson 
    | OpenFile


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["download", name, version] -> do
            path <- downloadPackage name version
            putErr $ "Source location: " ++ path

        ["inspect-hie", srcFile] -> do
            mHiePath <- HR.findHieFile srcFile
            case mHiePath of
                Nothing -> putErr $ "No .hie file found for " ++ srcFile
                Just hiePath -> do
                    putErr $ "Found .hie file: " ++ hiePath
                    HR.inspectHieFile hiePath

        ["find-symbol", srcFile, lineStr, colStr] -> do
            let line = read lineStr :: Int
            let col = read colStr :: Int
            mHiePath <- HR.findHieFile srcFile
            case mHiePath of
                Nothing -> putErr $ "No .hie file found for " ++ srcFile
                Just hiePath -> HR.findSymbolAt hiePath line col

        ["test-module-lookup", packageName, version, moduleName] -> do
            packageDir <- downloadPackage packageName version
            putErr $ "Looking for module: " ++ moduleName
            mFilePath <- findModuleFile packageDir moduleName
            case mFilePath of
                Nothing -> putErr $ "Module not found: " ++ moduleName
                Just filePath -> putErr $ "Found module at: " ++ filePath

        ["goto", srcFile, lineStr, colStr] -> gotoDefinition srcFile (read lineStr) (read colStr) OpenFile

        ["goto-print", srcFile, lineStr, colStr] -> gotoDefinition srcFile (read lineStr) (read colStr) Print

        ["goto-json", srcFile, lineStr, colStr] -> gotoDefinition srcFile (read lineStr) (read colStr) PrintJson

        _ -> putErr "Usage:\n\
                        \  hls-lookup download <package> <version>\n\
                        \  hls-lookup inspect-hie <source-file>\n\
                        \  hls-lookup find-symbol <source-file> <line> <col>\n\
                        \  hls-lookup test-module-lookup <package> <version> <module>\n\
                        \  hls-lookup goto <source-file> <line> <col>        - Find and open definition in VSCode\n\
                        \  hls-lookup goto-print <source-file> <line> <col>  - Find and print definition location\n\
                        \  hls-lookup goto-json <source-file> <line> <col>   - Find and print definition location as json"

-- | Main goto definition workflow
gotoDefinition :: FilePath -> Int -> Int -> GotoAction -> IO ()
gotoDefinition srcFile line col gotoAction = do
    -- Step 1: Find the .hie file
    hiePath <- findHieFileOrFail srcFile
    -- Step 2: Load HIE file and get symbols at position
    hieFile <- HR.loadHieFile hiePath
    putErr $ "Finding symbol at " ++ show line ++ ":" ++ show col
    let symbols = HR.getSymbolsAtPosition hieFile line col
    case reverse symbols of
        [] -> do
            putErr "Error: No symbol found at the specified position"
            exitFailure
        (sym:_) -> do
            putErr $ "Found symbol: " ++ sym.name
            putErr $ "  Module: " ++ fromMaybe "<unknown>" sym.symModule
            case (sym.symModule, sym.packageName, sym.packageVersion) of
                (Just modName, Just pkgName, Just pkgVer) -> do
                    putErr $ "  Package: " ++ pkgName ++ "-" ++ pkgVer

                    -- Step 3: Download the package
                    putErr "\nDownloading package..."
                    packageDir <- downloadPackage pkgName pkgVer
                    putErr $ "Package cached at: " ++ packageDir

                    continueWithPackage packageDir modName sym.name pkgName gotoAction
                _ -> do
                    putErr "Error: Could not determine package information"
                    putErr $ "  Raw Unit ID: " ++ fromMaybe "<none>" sym.rawUnitId
                    exitFailure
    where
        findHieFileOrFail srcFile = do
            putErr $ "Looking for HIE file for: " ++ srcFile
            mHiePath <- HR.findHieFile srcFile
            case mHiePath of
                Nothing -> do
                    putErr $ "Error: No .hie file found for " ++ srcFile
                    putErr "Hint: Make sure the project is compiled with -fwrite-ide-info"
                    exitFailure
                Just hiePath -> do
                    putErr $ "Found HIE file: " ++ hiePath
                    pure hiePath

-- | Continue the goto workflow with the downloaded package
continueWithPackage :: FilePath -> String -> String -> String -> GotoAction -> IO ()
continueWithPackage packageDir moduleName symbolName packageName gotoAction = do
    -- Step 4: Find the module file
    putErr $ "\nLooking for module: " ++ moduleName
    mModuleFile <- findModuleFile packageDir moduleName
    case mModuleFile of
        Nothing -> do
            putErr $ "Error: Could not find module file for " ++ moduleName
            exitFailure
        Just moduleFile -> do
            putErr $ "Found module at: " ++ moduleFile

            -- Step 5: Find the definition in the file
            putErr $ "\nSearching for definition of: " ++ symbolName
            mDefLoc <- findDefinition moduleFile symbolName
            (line, col) <- case mDefLoc of
                                Nothing -> do
                                    putErr $ "Warning: Could not find definition of " ++ symbolName ++ " in " ++ moduleFile
                                    putErr "Opening file at top..."
                                    pure (1,1)
                                Just defLoc -> do
                                    putErr $ "Found definition at line " ++ show defLoc.line ++ ", column " ++ show defLoc.column
                                    pure (defLoc.line, defLoc.column)
            case gotoAction of
                OpenFile -> openInVSCode moduleFile line col
                Print -> printLocation moduleFile line col
                PrintJson -> printJson moduleFile line col symbolName packageName

printJson :: FilePath -> Int -> Int -> String -> String -> IO ()
printJson moduleFile line col symbolName packageName =
    BS.putStrLn $ Aeson.encode 
        (GotoResponse 
            True 
            (Just moduleFile) 
            (Just line) 
            (Just col) 
            (Just symbolName)  
            (Just packageName)
            "Symbol found!")


-- | Open a file in VSCode at a specific line and column
openInVSCode :: FilePath -> Int -> Int -> IO ()
openInVSCode filePath line col = do
    let location = filePath ++ ":" ++ show line ++ ":" ++ show col
    putErr $ "\nOpening in VSCode: " ++ location
    callCommand $ "code --goto " ++ show location

-- | Print the file location (for scripting/integration)
printLocation :: FilePath -> Int -> Int -> IO ()
printLocation filePath line col = do
    putErr "\n=== DEFINITION LOCATION ==="
    putErr $ filePath ++ ":" ++ show line ++ ":" ++ show col
