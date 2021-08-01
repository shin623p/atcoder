-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as VU
import Control.Monad
import Data.Maybe
-- import qualified Data.Graph as G
import qualified Data.IntMap as IM
-- import qualified Data.IntSet as IS
-- import Data.List
-- import qualified Data.Set as Set
-- import qualified Data.Tree as T


main :: IO ()
main = do
  n <- fst . fromJust . BS.readInt <$> BS.getLine
  as <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  bs <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  cs <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ solve as bs cs


-- | solve for ABC202-C
-- >>> solve [1,2,2] [3,1,2] [2,3,2]
-- 4
--
solve :: [Int] -> [Int] -> [Int] -> Int
solve as bs cs = sum [bs' VU.! (c - 1) | c <- cs]
  where
    as' = IM.fromListWith (+) [(a, 1) | a <- as]
    bs' = VU.fromList [IM.findWithDefault 0 b as' | b <- bs]
