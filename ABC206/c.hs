module Main where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Control.Monad
import qualified Data.IntMap as IM
-- import Data.List
-- import GHC.Exts


main :: IO ()
main = do
  n <- fst . fromJust . BS.readInt <$> BS.getLine
  as <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ solve n as


-- | solve for ABC206-B
-- >>> solve 3 [1, 7, 1]
-- 2
--
-- >>> solve 10 [1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000]
-- 45
--
-- >>> solve 20 [7, 8, 1, 1, 4, 9, 9, 6, 8, 2, 4, 1, 1, 9, 5, 5, 5, 3, 6, 4]
-- 173
--
solve :: Int -> [Int] -> Int
solve n as = comb n - f as


-- |
-- >>> comb 10
-- 45
--
-- >>> comb 20
-- 190
--
comb :: Int -> Int
comb 1 = 0
comb n = (n - 1) * n `div` 2


-- |
-- >>> f [1, 2, 3, 2, 3, 4]
-- 2
--
-- >>> f [7, 8, 1, 1, 4, 9, 9, 6, 8, 2, 4, 1, 1, 9, 5, 5, 5, 3, 6, 4]
-- 17
--
f :: [Int] -> Int
f as = IM.foldr (\a b -> comb a + b) 0 $ IM.fromListWith (+) $ map (flip (,) 1) as


