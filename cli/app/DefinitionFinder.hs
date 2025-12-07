module DefinitionFinder
  ( findDefinition,
    DefinitionLocation (..),
  )
where

import Data.Char (isAlphaNum, isSpace)
import qualified Data.List as L

-- | Location of a definition in a file
data DefinitionLocation = DefinitionLocation
  { line :: Int, -- 1-indexed line number
    column :: Int -- 1-indexed column number
  }
  deriving (Show, Eq)

-- | Find the definition of a symbol in a Haskell source file
-- Returns the line and column number if found
findDefinition :: FilePath -> String -> IO (Maybe DefinitionLocation)
findDefinition filePath symbolName = do
  content <- readFile filePath
  let linesWithNum = L.zip [1 ..] (lines content)
  pure $ findInLines symbolName linesWithNum

-- | Search for a symbol definition in numbered lines
findInLines :: String -> [(Int, String)] -> Maybe DefinitionLocation
findInLines symbolName linesWithNum =
  case L.filter (isDefinitionLine symbolName . snd) linesWithNum of
    [] -> Nothing
    ((lineNum, lineContent) : _) ->
      Just $
        DefinitionLocation
          { line = lineNum,
            column = findColumn symbolName lineContent
          }

-- | Check if a line contains a definition of the symbol
isDefinitionLine :: String -> String -> Bool
isDefinitionLine symbolName lineContent =
  let trimmed = L.dropWhile isSpace lineContent
   in or
        [ isTypeSignature symbolName trimmed,
          isFunctionDefinition symbolName trimmed,
          isDataConstructor symbolName trimmed,
          isTypeDefinition symbolName trimmed,
          isNewtypeDefinition symbolName trimmed,
          isClassDefinition symbolName trimmed,
          isInstanceDefinition symbolName trimmed
        ]

-- | Check for type signature: "symbolName :: "
isTypeSignature :: String -> String -> Bool
isTypeSignature symbolName line =
  let pattern = symbolName ++ " ::"
   in pattern `L.isPrefixOf` line && checkBoundary symbolName line

-- | Check for function definition: "symbolName = " or "symbolName x y = "
isFunctionDefinition :: String -> String -> Bool
isFunctionDefinition symbolName line =
  case words line of
    [] -> False
    (firstWord : rest) ->
      firstWord == symbolName
        && checkBoundary symbolName line
        && (null rest || "=" `L.elem` rest || L.notElem '=' (unwords rest))

-- | Check for data constructor: "data Foo = SymbolName" or "| SymbolName"
isDataConstructor :: String -> String -> Bool
isDataConstructor symbolName line =
  case words line of
    ("data" : _ : rest) -> L.any (symbolNameMatches symbolName) rest
    ("|" : name : _) -> symbolNameMatches symbolName name
    _ -> False

-- | Check for type definition: "type SymbolName = "
isTypeDefinition :: String -> String -> Bool
isTypeDefinition symbolName line =
  case words line of
    ("type" : name : _) -> symbolNameMatches symbolName name
    _ -> False

-- | Check for newtype definition: "newtype SymbolName = "
isNewtypeDefinition :: String -> String -> Bool
isNewtypeDefinition symbolName line =
  case words line of
    ("newtype" : name : _) -> symbolNameMatches symbolName name
    _ -> False

-- | Check for class definition: "class SymbolName" or "class (Context) => SymbolName"
isClassDefinition :: String -> String -> Bool
isClassDefinition symbolName line =
  case words line of
    ("class" : rest) -> L.any (symbolNameMatches symbolName) (removeContext rest)
    _ -> False

-- | Check for instance definition: "instance SymbolName" or "instance (Context) => SymbolName"
isInstanceDefinition :: String -> String -> Bool
isInstanceDefinition symbolName line =
  case words line of
    ("instance" : rest) -> L.any (symbolNameMatches symbolName) (removeContext rest)
    _ -> False

-- | Remove context from type class or instance declaration
-- Drops everything before "=>" if present
removeContext :: [String] -> [String]
removeContext ws = case L.dropWhile (/= "=>") ws of
  [] -> ws
  (_ : rest) -> rest

-- | Check if symbol name matches, handling type applications
symbolNameMatches :: String -> String -> Bool
symbolNameMatches symbolName word =
  let cleaned = L.takeWhile (\c -> isAlphaNum c || c == '_' || c == '\'') word
   in cleaned == symbolName

-- | Check that the symbol is a whole word (not part of a larger identifier)
checkBoundary :: String -> String -> Bool
checkBoundary symbolName line =
  case dropWhile isSpace line of
    "" -> False
    str ->
      let afterSymbol = drop (length symbolName) str
       in symbolName `L.isPrefixOf` str
            && case afterSymbol of
              [] -> True
              (c : _) -> not (isAlphaNum c || c == '_' || c == '\'')

-- | Find the column number where the symbol starts
findColumn :: String -> String -> Int
findColumn _ lineContent =
  let spaces = L.length (L.takeWhile isSpace lineContent)
   in spaces + 1 -- 1-indexed
