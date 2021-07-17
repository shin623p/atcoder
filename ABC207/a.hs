module Main where

import Data.List
-- | ABC208 A
--
-- >>> solve [2, 4, 5]
-- 9
--
-- >>> solve [6, 6, 6]
-- 12
--
-- >>> solve [99, 99, 98]
-- 198
--
solve :: [Int] -> Int
solve ns = sum $ take 2 $ sortBy (flip compare) ns

main :: IO ()
main = getLine >>= (putStrLn . show . solve . map read . words)

