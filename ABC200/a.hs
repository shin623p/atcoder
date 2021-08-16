-- {-# LANGUAGE OverloadedStrings #-}

module Main where

-- import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Vector.Unboxed as VU
-- import Control.Monad
-- import Data.Maybe
-- import qualified Data.Graph as G
-- import qualified Data.IntMap as IM
-- import qualified Data.IntSet as IS
-- import Data.List
-- import Data.Ord
-- import qualified Data.Set as Set
-- import qualified Data.Tree as T

main = do
  n <- readLn
  print $ solve n

-- | solver for ABC200-A
-- >>> solve 2021
-- 21
-- >>> solve 200
-- 2
-- 
solve :: Int -> Int
solve n = (n - 1) `div` 100 + 1
