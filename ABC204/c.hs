-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Data.Maybe
import qualified Data.Graph as G
-- import qualified Data.IntMap as IM
-- import qualified Data.IntSet as IS
-- import Data.List
-- import qualified Data.Set as Set
-- import qualified Data.Tree as T


main :: IO ()
main = do
  [n, m] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  xs <- map (map (fst . fromJust . BS.readInt) . BS.words) <$> replicateM m BS.getLine
  print $ solve n m xs


-- | solve for ABC204-C
-- >>> solve 3 3 [[1,2],[2,3],[3,2]]
-- 7
--
-- >>> solve 3 0 []
-- 3
--
-- >>> solve 4 4 [[1,2],[2,3],[3,4],[4,1]]
-- 16
--
solve :: Int -> Int -> [[Int]] -> Int
solve n m xs = sum $ map (length . G.reachable (G.buildG (1, n) $ map (\[a, b] -> (a, b)) xs)) [1..n]


