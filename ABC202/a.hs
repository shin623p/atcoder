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
  xs <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ solve xs


-- | solver for ABC200-B
-- >>> solve [1, 4, 3]
-- 13
-- >>> solve [5, 6, 4]
-- 6
-- 
solve :: [Int] -> Int
solve = (21 -) . sum
