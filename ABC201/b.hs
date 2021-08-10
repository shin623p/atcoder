-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Vector.Unboxed as VU
import Control.Monad
import Data.Maybe
-- import qualified Data.Graph as G
-- import qualified Data.IntMap as IM
-- import qualified Data.IntSet as IS
import Data.List
import Data.Ord
-- import qualified Data.Set as Set
-- import qualified Data.Tree as T

main = do
  n <- fst . fromJust . BS.readInt <$> BS.getLine
  xs <- map ((\[s, t] -> ((fst . fromJust . BS.readInt) t, s)) . BS.words) <$> replicateM n BS.getLine
  BS.putStrLn $ solve xs



solve :: [(Int, BS.ByteString)] -> BS.ByteString
solve xs = snd $ head $ tail $ sortBy (comparing Down) xs
