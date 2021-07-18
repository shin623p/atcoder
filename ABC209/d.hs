module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Control.Monad


main :: IO ()
main = do
  [n, q] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  rs <- map (map (fst . fromJust . BS.readInt) . BS.words) <$> replicateM (n - 1) BS.getLine
  ts <- map (map (fst . fromJust . BS.readInt) . BS.words) <$> replicateM q BS.getLine
  solve rs ts


-- | solve for ABC209-D 
-- >>> solve [[1, 2], [2, 3], [2, 4]] [[1, 2]]
-- Road
--
-- >>> solve [[1, 2], [2, 3], [3, 4], [4, 5]] [[1, 3], [1, 5]]
-- Town
-- Town
--
-- >>> solve [[2, 3], [5, 6], [4, 8], [8, 9], [4, 5], [3, 4], [1, 9], [3, 7]] [[7, 9], [2, 5], [2, 6], [4, 6], [2, 4], [5, 8], [7, 8], [3, 6], [5, 6]]
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
solve :: [[Int]] -> [[Int]] -> IO ()
solve roads starts = do
  let [townL, townR] = townGroup $ sol1 roads [] []
  let c = chk townL starts
  mapM_ (\[c', d'] -> if xor c' d' then putStrLn "Road" else putStrLn "Town") c
  where
    townGroup = map Set.fromList . foldl (\[ls, rs] [l, r] -> [l:ls, r:rs]) [[], []]
    chk townL = map (\[c, d] -> [Set.member c townL, Set.member d townL])


-- | sol1
-- >>> sort $ sol1 [[1, 2], [2, 3], [2, 4]] [] []
-- [[1,2],[3,2],[4,2]]
--
-- >>> sort $ sol1 [[1, 2], [2, 3], [3, 4], [4, 5]] [] []
-- [[1,2],[3,2],[3,4],[5,4]]
--
-- >>> sort $ sol1 [[2, 3], [5, 6], [4, 8], [8, 9], [4, 5], [3, 4], [1, 9], [3, 7]] [] []
-- [[2,3],[4,3],[4,5],[4,8],[6,5],[7,3],[9,1],[9,8]]
sol1 :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
sol1 [] wk acc = wk ++ acc
sol1 ([a, b]:rs) [] acc = (\(fixRoads, restRoads) -> sol1 restRoads fixRoads ([a, b]:acc)) $ sol2 a b rs 
sol1 rs ([a, b]:xs) acc = (\(fixRoads, restRoads) -> sol1 restRoads (fixRoads ++ xs) ([a, b]:acc)) $ sol2 a b rs 
sol1 _ _ _ = undefined


-- | sol2
-- >>> sol2 2 3 [[5, 6], [4, 8], [8, 9], [4, 5], [3, 4], [1, 9], [3, 7]]
-- ([[7,3],[4,3]],[[1,9],[4,5],[8,9],[4,8],[5,6]])
sol2 :: Int -> Int -> [[Int]] -> ([[Int]], [[Int]])
sol2 a b = foldl f ([], [])
  where
    f (fixRoads, restRoads) [a', b']
      | a == a' || b == b' = ([a', b']:fixRoads, restRoads)
      | a == b' || b == a' = ([b', a']:fixRoads, restRoads)
      | otherwise = (fixRoads, [a', b']:restRoads)
    f _ _ = undefined


xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor _ _ = True
