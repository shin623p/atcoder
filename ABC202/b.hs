{-# LANGUAGE OverloadedStrings #-}

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
main = BS.putStrLn . BS.reverse . solve =<< BS.getLine

-- | solve for ABC202-B
-- >>> solve ("0601889"
-- "6881090"
--
-- >>> solve "86910"
-- "01698"
--
-- >>> solve "01010"
-- "01010"
--
solve :: BS.ByteString -> BS.ByteString
solve "" = ""
solve xs
  | x == '6' = BS.cons '9' $ solve xs'
  | x == '9' = BS.cons '6' $ solve xs'
  | otherwise = BS.cons x $ solve xs'
  where
    x = BS.head xs
    xs' = BS.tail xs
