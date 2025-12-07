module Util (splitOn, putErr) where

import qualified Data.List as L
import System.IO ( hPutStrLn, stderr )


-- | Split string on a character
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s = case L.break (== c) s of
  (chunk, "") -> [chunk]
  (chunk, _ : rest) -> chunk : splitOn c rest

putErr :: String -> IO ()
putErr = hPutStrLn stderr