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
  [n, k] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ solve n k


-- | solver for ABC200-B
-- >>> solve 2021 4
-- 50531
-- >>> solve 40000 2
-- 1
-- >>> solve 8691 20
-- 84875488281
-- 
solve :: Int -> Int -> Int
solve n 0 = n
solve n k
  | n `mod` 200 == 0 = solve (n `div` 200) (k - 1)
  | otherwise = solve (n * 1000 + 200) (k - 1)

