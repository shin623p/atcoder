{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Control.Monad
-- import qualified Data.IntMap as IM
-- import Data.List
-- import GHC.Exts


main :: IO ()
main = do
  [a, b, c] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  putStrLn $ BS.unpack $ solve a b c


-- | solve for ABC206-B
-- >>> solve 3 2 4
-- ">"
--
-- >>> solve (-7) 7 2
-- "="
--
-- >>> solve (-8) 6 3
-- "<"
--
-- >>> solve (-1000000000) 1000000000 1000000000
-- "="
--
solve :: Int -> Int -> Int -> BS.ByteString
solve a b c
  | even c && abs a > abs b = ">"
  | even c && abs a < abs b = "<"
  | even c && abs a == abs b = "="
  | odd c && a > b = ">"
  | odd c && a < b = "<"
  | otherwise = "="
  
