module Main where

import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Data.Maybe
import qualified Data.Graph as G
-- import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
-- import Data.List
-- import qualified Data.Set as Set
import qualified Data.Tree as T


main :: IO ()
main = do
  [n, q] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  rs <- map (map (fst . fromJust . BS.readInt) . BS.words) <$> replicateM (n - 1) BS.getLine
  ts <- map (map (fst . fromJust . BS.readInt) . BS.words) <$> replicateM q BS.getLine
  solve n rs ts

-- | solve for ABC209-D
-- >>> solve 4 [[1, 2], [2, 3], [2, 4]] [[1, 2]]
-- Road
--
-- >>> solve 5 [[1, 2], [2, 3], [3, 4], [4, 5]] [[1, 3], [1, 5]]
-- Town
-- Town
--
-- >>> solve 9 [[2, 3], [5, 6], [4, 8], [8, 9], [4, 5], [3, 4], [1, 9], [3, 7]] [[7, 9], [2, 5], [2, 6], [4, 6], [2, 4], [5, 8], [7, 8], [3, 6], [5, 6]]
-- Town
-- Road
-- Town
-- Town
-- Town
-- Town
-- Road
-- Road
-- Road
--
solve :: Int -> [[Int]] -> [[Int]] -> IO ()
solve n roads starts = do
  let lvl = T.levels $ head $ G.components (G.buildG (1, n) $ f roads)
  let evenLvls = snd . foldl (\(l, x') x -> (xor l True, if l then IS.union x' x else x')) (True, IS.empty) . map IS.fromList $ lvl
  mapM_ (\[c, d] -> putStrLn $ chk evenLvls c d) starts
  where
    chk l c d
      | IS.member c l `xor` IS.member d l = "Road"
      | otherwise = "Town"

-- | helper func
-- >>> f [[1, 2], [2, 3], [2, 4]]
-- [(1,2),(2,3),(2,4)]
f :: [[Int]] -> [(Int, Int)]
f = map (\[a, b] -> (a, b))


xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor _ _ = True
