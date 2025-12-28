module DefinitionFinder
  ( findDefinition,
    DefinitionLocation (..),
  )
where

import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (isSpace, isAlphaNum)

-- | Location of a definition in a file
data DefinitionLocation = DefinitionLocation
  { line :: Int, -- 1-indexed line number
    column :: Int -- 1-indexed column number
  }
  deriving (Show, Eq)

-- | Find the definition of a symbol in a Haskell source file
-- Returns the line and column number if found
findDefinition :: FilePath -> Text -> IO (Maybe DefinitionLocation)
findDefinition filePath symbolName = do
  content <- TIO.readFile filePath
  let linesWithNum = L.zip [1 ..] (T.lines content)
  pure $ findInLines symbolName linesWithNum

-- | Search for a symbol definition in numbered lines
findInLines :: Text -> [(Int, Text)] -> Maybe DefinitionLocation
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
isDefinitionLine :: Text -> Text -> Bool
isDefinitionLine symbolName lineContent =
  let trimmed = T.dropWhile isSpace lineContent
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
isTypeSignature :: Text -> Text -> Bool
isTypeSignature symbolName line =
  let pattern = symbolName <> " ::"
   in pattern `T.isPrefixOf` line && checkBoundary symbolName line

-- | Check for function definition: "symbolName = " or "symbolName x y = "
isFunctionDefinition :: Text -> Text -> Bool
isFunctionDefinition symbolName line =
  case T.words line of
    [] -> False
    (firstWord : rest) ->
      firstWord == symbolName
        && checkBoundary symbolName line
        && (null rest || "=" `elem` rest || not (T.elem '=' (T.unwords rest)))

-- | Check for data constructor: "data Foo = SymbolName" or "| SymbolName"
isDataConstructor :: Text -> Text -> Bool
isDataConstructor symbolName line =
  case T.words line of
    ("data" : _ : rest) -> L.any (symbolNameMatches symbolName) rest
    ("|" : name : _) -> symbolNameMatches symbolName name
    _ -> False

-- | Check for type definition: "type SymbolName = "
isTypeDefinition :: Text -> Text -> Bool
isTypeDefinition symbolName line =
  case T.words line of
    ("type" : name : _) -> symbolNameMatches symbolName name
    _ -> False

-- | Check for newtype definition: "newtype SymbolName = "
isNewtypeDefinition :: Text -> Text -> Bool
isNewtypeDefinition symbolName line =
  case T.words line of
    ("newtype" : name : _) -> symbolNameMatches symbolName name
    _ -> False

-- | Check for class definition: "class SymbolName" or "class (Context) => SymbolName"
isClassDefinition :: Text -> Text -> Bool
isClassDefinition symbolName line =
  case T.words line of
    ("class" : rest) -> L.any (symbolNameMatches symbolName) (removeContext rest)
    _ -> False

-- | Check for instance definition: "instance SymbolName" or "instance (Context) => SymbolName"
isInstanceDefinition :: Text -> Text -> Bool
isInstanceDefinition symbolName line =
  case T.words line of
    ("instance" : rest) -> L.any (symbolNameMatches symbolName) (removeContext rest)
    _ -> False

-- | Remove context from type class or instance declaration
-- Drops everything before "=>" if present
removeContext :: [Text] -> [Text]
removeContext ws = case L.dropWhile (/= "=>") ws of
  [] -> ws
  (_ : rest) -> rest

-- | Check if symbol name matches, handling type applications
symbolNameMatches :: Text -> Text -> Bool
symbolNameMatches symbolName word =
  let cleaned = T.takeWhile (\c -> isAlphaNum c || c == '_' || c == '\'') word
   in cleaned == symbolName

-- | Check that the symbol is a whole word (not part of a larger identifier)
checkBoundary :: Text -> Text -> Bool
checkBoundary symbolName line =
  case T.dropWhile isSpace line of
    "" -> False
    str ->
      let afterSymbol = T.drop (T.length symbolName) str
       in symbolName `T.isPrefixOf` str
            && case T.unpack afterSymbol of
              [] -> True
              (c : _) -> not (isAlphaNum c || c == '_' || c == '\'')

-- | Find the column number where the symbol starts
findColumn :: Text -> Text -> Int
findColumn _ lineContent =
  let spaces = T.length (T.takeWhile isSpace lineContent)
   in spaces + 1 -- 1-indexed
