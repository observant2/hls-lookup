module Util (splitOn, putErr) where

import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import System.IO ( hPutStrLn, stderr )


-- | Split string on a character
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s = case L.break (== c) s of
  (chunk, "") -> [chunk]
  (chunk, _ : rest) -> chunk : splitOn c rest

putErr :: Text -> IO ()
putErr t = hPutStrLn stderr (T.unpack t)