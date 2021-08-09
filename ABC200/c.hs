-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Vector.Unboxed as VU
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
  _ <- BS.getLine
  as <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ solve as

-- | solve for ABC200-C
-- >>> solve [123, 223, 123, 523, 200, 2000]
-- 4
--
-- >>> solve [1, 2, 3, 4, 5]
-- 0
--
-- >>> solve [199, 100, 200, 400, 300, 500, 600, 200]
-- 9
--
solve :: [Int] -> Int
solve xs = sum $ map (cmb . (\x -> IM.findWithDefault 0 x cnt)) [0..199]
  where
    cnt = IM.fromListWith (+) $ map ((flip (,) 1) . (`mod` 200)) xs
    cmb x = x * (x - 1) `div` 2
