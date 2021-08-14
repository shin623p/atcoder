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
  [r, x, y] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ solve r x y


-- | solver for ABC200-B
-- >>> solve 5 15 0
-- 3
-- >>> solve 5 11 0
-- 3
-- >>> solve 3 4 4
-- 2
-- 
solve :: Int -> Int -> Int -> Int
solve r x y
  | n < 1 = 2
  | otherwise = ceiling n
  where
    n = sqrt (fromIntegral (x * x + y * y)) / fromIntegral r
