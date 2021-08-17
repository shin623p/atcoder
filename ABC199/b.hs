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
  _ <- BS.getLine
  as <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  bs <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ solve as bs

-- | solver for ABC199-B
-- >>> solve [3, 2] [7, 5]
-- 3
-- >>> solve [1, 5, 3] [10, 7, 3]
-- 0
-- >>> solve [3, 2, 5] [6, 9, 8]
-- 2
-- 
solve :: [Int] -> [Int] -> Int
solve as bs
  | a > b = 0
  | otherwise = b - a + 1
  where
    a = maximum as
    b = minimum bs
