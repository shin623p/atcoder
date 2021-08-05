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
-- import qualified Data.Set as Set
-- import qualified Data.Tree as T


main :: IO ()
main = do
  [n, k] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ solve n k

-- | solve for ABC203-B
-- >>> solve 1 2
-- 203
--
-- >>> solve 3 3
-- 1818
--
solve :: Int -> Int -> Int
solve 0 _ = 0
solve n k = sum [ n' + 1 .. n' + k] + solve (n - 1) k
  where
    n' = n * 100
