module Main where

import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Set as Set
-- import Data.List
import Data.Maybe
-- import Control.Monad
-- import GHC.Exts


main :: IO ()
main = do
  xs <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ solve xs


-- | solve for ABC207-C 
-- >>> solve [5, 2, 3, 2]
-- 2
--
-- >>> solve [6, 9, 2, 3]
-- -1
--
solve :: [Int] -> Int
solve [a, b, c, d] = sol a b c d
solve _ = undefined

sol :: Int -> Int -> Int -> Int -> Int
sol a b c d
  | c*d-b>0 = div (a+c*d-b-1) (c*d-b)
  | otherwise = -1
