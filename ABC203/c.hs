-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Vector.Unboxed as VU
import Control.Monad
import Data.Maybe
-- import qualified Data.Graph as G
-- import qualified Data.IntMap as IM
-- import qualified Data.IntSet as IS
import Data.List
-- import qualified Data.Set as Set
-- import qualified Data.Tree as T


main :: IO ()
main = do
  [n, k] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  xs <- map ((\[a, b] -> (a, b)) . (map (fst . fromJust . BS.readInt) . BS.words)) <$> replicateM n BS.getLine
  print $ solve k xs


-- | solve for ABC203-C
-- >>> solve 3 [(2,1),(5,10)]
-- 4
--
-- >>> solve 1000000000 [(1, 1000000000), (2, 1000000000), (3, 1000000000), (4, 1000000000), (5, 1000000000)]
-- 6000000000
--
-- >>> solve 2 [(5,5), (2,1), (2,2)]
-- 10
--
solve :: Int -> [(Int, Int)] -> Int
solve k xs = solve1 k xs'
  where
    xs' = sort xs


solve1 :: Int -> [(Int, Int)] -> Int
solve1 k [] = k
solve1 k ((a', b'):xs)
  | a' > k = k
  | otherwise = solve1 (k + b') xs
