{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Vector.Unboxed as VU
import Control.Monad
import Data.Maybe
-- import qualified Data.Graph as G
-- import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
-- import Data.List
-- import qualified Data.Set as Set
-- import qualified Data.Tree as T


main :: IO ()
main = do
  n <- fst . fromJust . BS.readInt <$> BS.getLine
  as <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  BS.putStrLn $ solve n as

-- | solve for ABC205-B
-- >>> solve 5 [3, 1, 2, 4, 5]
-- "Yes"
--
-- >>> solve 6 [3, 1, 4, 1, 5, 2]
-- "No"
--
-- >>> solve 3 [1, 2, 3]
-- "Yes"
--
solve :: Int -> [Int] -> BS.ByteString
solve n as
  | l == n = "Yes"
  | otherwise = "No"
  where
    l = (IS.size . IS.fromList) as
