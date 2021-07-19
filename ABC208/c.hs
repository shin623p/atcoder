module Main where

import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Set as Set
import Data.List
import Data.Maybe
-- import Control.Monad
import GHC.Exts

main :: IO ()
main = do
  [n, k] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  as <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  solve n k as


-- | solve for ABC208-C 
-- >>> solve 2 7 [1, 8]
-- 4
-- 3
--
-- >>> solve 1 3 [33]
-- 3
--
-- >>> solve 7 1000000000000 [99, 8, 2, 4, 43, 5, 3]
-- 142857142857
-- 142857142857
-- 142857142858
-- 142857142857
-- 142857142857
-- 142857142857
-- 142857142857
-- 
solve :: Int -> Int -> [Int] -> IO ()
solve n k as = mapM_ print as'
  where
    c = k `div` n
    d = k `mod` n
    as' = map snd $ sort $ map (\(x, (_, i)) -> (i, if x < d then c + 1 else c)) $ sortWith (snd . snd) $ zip [0..] $ sort $ zip as [0..]

