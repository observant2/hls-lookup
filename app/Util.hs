module Util (splitOn) where

import qualified Data.List as L


-- | Split string on a character
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s = case L.break (== c) s of
  (chunk, "") -> [chunk]
  (chunk, _ : rest) -> chunk : splitOn c rest
