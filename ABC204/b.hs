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
  _ <- BS.getLine
  as <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ solve as

-- | solve for ABC204-B
-- >>> solve [6, 17, 28]
-- 25
--
-- >>> solve [8, 9, 10, 11]
-- 1
--
solve :: [Int] -> Int
solve [] = 0
solve (a:as)
  | a < 11 = 0 + solve as
  | otherwise = (a - 10) + solve as
