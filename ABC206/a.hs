{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Control.Monad
-- import qualified Data.IntMap as IM
-- import Data.List
-- import GHC.Exts


main :: IO ()
main = do
  n <- fst . fromJust . BS.readInt <$> BS.getLine
  putStrLn $ BS.unpack $ solve n


-- | solve for ABC206-A
-- >>> solve 180
-- "Yay!"
--
-- >>> solve 200
-- ":("
--
-- >>> solve 191
-- "so-so"
--
solve :: Int -> BS.ByteString
solve n
  | n' < 206 = "Yay!"
  | n' > 206 = ":("
  | otherwise = "so-so"
  where
    n' = n + (n * 8 `div` 100)

