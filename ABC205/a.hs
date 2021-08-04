module Main where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Control.Monad
-- import qualified Data.IntMap as IM
-- import Data.List
-- import GHC.Exts


main :: IO ()
main = do
  [a, b] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ solve a b


-- | solve for ABC205-A
-- >>> solve 45 200
-- 90.0
--
-- >>> solve 37 450
-- 166.5
--
-- >>> solve 0 1000
-- 0.0
--
solve :: Int -> Int -> Double
solve a b = fromIntegral (a * b) / 100
