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
  [x, y] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ solve x y

-- | solve for ABC204-A
-- >>> solve 0 1
-- 2
--
-- >>> solve 0 0
-- 0
--
solve :: Int -> Int -> Int
solve x y
  | x == y = x
  | otherwise = 3 - x - y
