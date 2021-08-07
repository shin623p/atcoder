module Main where

-- import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Vector.Unboxed as VU
-- import Control.Monad
-- import Data.Maybe
-- import qualified Data.Graph as G
-- import qualified Data.IntMap as IM
-- import qualified Data.IntSet as IS
-- import Data.List
-- import qualified Data.Set as Set
-- import qualified Data.Tree as T


main :: IO ()
main = print . solve =<< getLine

-- | solve for ABC201-C
-- >>> solve "ooo???xxxx"
-- 108
--
-- >>> solve "o?oo?oxoxo"
-- 0
--
-- >>> solve "xxxxx?xxxo"
-- 15
--
-- >>> solve "??????????"
-- 10000
-- 
solve :: String -> Int
solve xs
  | length o > 4 = 0
  | null nx = 0
  | otherwise = length [ (a, b, c, d) |
                         a <- nx,
                         b <- nx,
                         c <- nx,
                         d <- nx,
                         all (\i -> i `elem` [a, b, c, d]) o]
  where
    xs' = zip [0..9] xs
    o = filter ((== 'o') . snd) xs'
    nx = filter ((/= 'x') . snd) xs'
