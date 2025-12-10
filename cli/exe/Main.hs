module Main where

import qualified HieReader as HR
import PackageDownload (downloadPackage)
import System.Environment (getArgs)
import ModuleLookup (findModuleFile)
import DefinitionFinder (findDefinition, DefinitionLocation(..))
import System.Process (callCommand)
import System.Exit (exitFailure)
import Types (GotoResponse(GotoResponse))
import Data.Aeson as Aeson ( encode )
import qualified Data.ByteString.Lazy.Char8 as BS
import Util (putErr)
import Control.Monad.Except
    ( ExceptT, runExceptT, MonadError(throwError) )
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import qualified PlanLookup

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

-- | Parse unit ID to extract package name and version (fallback when plan.json unavailable)
-- Example: "tar-0.7.0.0-l-tar-internal-abc..." -> Just ("tar", "0.7.0.0")
parseUnitId :: String -> Maybe (String, String)
parseUnitId unitId =
    case words (map (\c -> if c == '-' then ' ' else c) unitId) of
        (pkg:ver:_) | all isVersionChar ver -> Just (pkg, ver)
        _ -> Nothing
  where
    isVersionChar c = c `elem` ('.':['0'..'9'])

-- | Main goto definition workflow
gotoDefinition :: FilePath -> Int -> Int -> GotoAction -> IO ()
gotoDefinition srcFile line col gotoAction = do
    result <- runExceptT $ do
        -- Step 1: Find the .hie file
        hiePath <- findHieFileE srcFile

        -- Step 2: Load HIE file and get symbols at position
        hieFile <- liftIO $ HR.loadHieFile hiePath
        liftIO $ putErr $ "Finding symbol at " ++ show line ++ ":" ++ show col
        let symbols = reverse $ HR.getSymbolsAtPosition hieFile line col

        -- Check if we found any symbols
        when (null symbols) $
            throwError "No symbol found at this position"

        -- Step 3: Get package info from plan.json (or fall back to HIE file)
        let firstSymbol = head symbols
            symName = firstSymbol.name

        -- Get module name (always needed)
        modName <- case firstSymbol.symModule of
            Just m -> pure m
            Nothing -> throwError "No module name in symbol info"

        -- Get package name and version
        (pkgName, pkgVer) <- case firstSymbol.rawUnitId of
            Just unitId -> do
                action <- liftIO $ PlanLookup.shouldDownloadPackage unitId
                case action of
                    PlanLookup.ShouldDownload name ver -> do
                        liftIO $ putErr $ "External Hackage package: " ++ name ++ "-" ++ ver
                        pure (name, ver)
                    PlanLookup.InternalPackage -> do
                        liftIO $ putErr $ "Skipping internal package: " ++ unitId
                        throwError "Not an external Hackage package - skipping"
                    PlanLookup.UseHie -> do
                        liftIO $ putErr "Plan.json not available, parsing from HIE file..."
                        -- Fall back to parsing unit ID
                        case parseUnitId unitId of
                            Just (name, ver) -> pure (name, ver)
                            Nothing -> throwError $ "Could not parse package info from Unit ID: " ++ unitId
            Nothing -> throwError "No unit ID in symbol info"

        -- Step 5: Download package
        liftIO $ putErr $ "Found symbol: " ++ symName
        liftIO $ putErr $ "  Module: " ++ modName
        liftIO $ putErr $ "  Package: " ++ pkgName ++ "-" ++ pkgVer
        liftIO $ putErr "\nDownloading package..."
        packageDir <- liftIO $ downloadPackage pkgName pkgVer
        liftIO $ putErr $ "Package cached at: " ++ packageDir

        -- Step 6: Find module file
        moduleFile <- findModuleFileE packageDir modName

        -- Step 7: Find definition
        liftIO $ putErr $ "\nSearching for definition of: " ++ symName
        mDefLoc <- liftIO $ findDefinition moduleFile symName
        let (defLine, defCol) = case mDefLoc of
                Nothing -> (1, 1)
                Just defLoc -> (defLoc.line, defLoc.column)

        -- Log result
        liftIO $ case mDefLoc of
            Nothing -> do
                putErr $ "Warning: Could not find definition of " ++ symName ++ " in " ++ moduleFile
                putErr "Opening file at top..."
            Just defLoc ->
                putErr $ "Found definition at line " ++ show defLoc.line ++ ", column " ++ show defLoc.column

        -- Return all the data
        pure (moduleFile, defLine, defCol, symName, pkgName)

    -- Handle the result
    case result of
        Left err ->
            case gotoAction of
                PrintJson -> printJsonFailure err  -- Exit with code 0 in JSON mode
                _ -> do
                    putErr err
                    exitFailure
        Right (file, line, col, sym, pkg) ->
            performAction gotoAction file line col sym pkg

-- | Find HIE file or fail with error
findHieFileE :: FilePath -> ExceptT String IO FilePath
findHieFileE srcFile = do
    liftIO $ putErr $ "Looking for HIE file for: " ++ srcFile
    mHiePath <- liftIO $ HR.findHieFile srcFile
    case mHiePath of
        Nothing -> do
            liftIO $ putErr "Hint: Make sure the project is compiled with -fwrite-ide-info"
            throwError $ "No .hie file found for " ++ srcFile
        Just hiePath -> do
            liftIO $ putErr $ "Found HIE file: " ++ hiePath
            pure hiePath

-- | Find module file or fail with error
findModuleFileE :: FilePath -> String -> ExceptT String IO FilePath
findModuleFileE packageDir moduleName = do
    liftIO $ putErr $ "\nLooking for module: " ++ moduleName
    mFile <- liftIO $ findModuleFile packageDir moduleName
    case mFile of
        Nothing -> throwError $ "Could not find module file for " ++ moduleName
        Just file -> do
            liftIO $ putErr $ "Found module at: " ++ file
            pure file

-- | Perform the action based on GotoAction
performAction :: GotoAction -> FilePath -> Int -> Int -> String -> String -> IO ()
performAction OpenFile file line col _ _ = openInVSCode file line col
performAction Print file line col _ _ = printLocation file line col
performAction PrintJson file line col sym pkg = printJson file line col sym pkg

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

printJsonFailure :: String -> IO ()
printJsonFailure message =
    BS.putStrLn $ Aeson.encode
        (GotoResponse False Nothing Nothing Nothing Nothing Nothing message)


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
