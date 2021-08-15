-- {-# LANGUAGE OverloadedStrings #-}

module Main where

-- import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Vector.Unboxed as VU
import Control.Monad
-- import Data.Maybe
-- import qualified Data.Graph as G
-- import qualified Data.IntMap as IM
-- import qualified Data.IntSet as IS
import Data.List
-- import Data.Ord
-- import qualified Data.Set as Set
-- import qualified Data.Tree as T

main = do
  as <- map read . words <$> getLine
  -- [r, x, y] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  putStrLn $ solve as

-- | solver for ABC201-A
-- >>> solve [5, 1, 3]
-- "Yes"
-- >>> solve [1, 4, 3]
-- "No"
-- >>> solve [5, 5, 5]
-- "Yes"
-- 
solve :: [Int] -> String
solve = f . sort
  where
    f (a:b:c:[])
      | a - b == b - c = "Yes"
      | otherwise = "No"
