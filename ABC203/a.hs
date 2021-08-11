-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Vector.Unboxed as VU
import Control.Monad
import Data.Maybe
-- import qualified Data.Graph as G
-- import qualified Data.IntMap as IM
-- import qualified Data.IntSet as IS
-- import Data.List
-- import Data.Ord
-- import qualified Data.Set as Set
-- import qualified Data.Tree as T

main = do
  [a, b, c] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ solve a b c


-- | solver for ABC203-A
-- >>> solve 2 5 2
-- 5
-- >>> solve 4 5 6
-- 0
-- >>> solve 1 1 1
-- 1
-- 
solve :: Int -> Int -> Int -> Int
solve a b c
  | a == b = c
  | a == c = b
  | b == c = a
  | otherwise = 0
