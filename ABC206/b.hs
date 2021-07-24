module Main where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe
-- import qualified Data.Set as Set
-- import Data.List
-- import Control.Monad
-- import GHC.Exts


main :: IO ()
main = do
  n <- fst . fromJust . BS.readInt <$> BS.getLine
  print $ solve n


-- | solve for ABC206-B
-- >>> solve 12
-- 5
--
-- >>> solve 100128
-- 447
--
solve :: Int -> Int
solve n = f 0 1
  where
    f n' i
      | n <= n' = i - 1
      | otherwise = f (n' + i) (i + 1)


