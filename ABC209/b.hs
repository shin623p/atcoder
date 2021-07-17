import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
-- import  Control.Monad

-- | ABC209 D
--
-- >>> solve [1, 3] 3
-- Yes
--
-- >>> solve [3, 3, 4, 4] 10
-- No
--
-- >>> solve [3, 1, 4, 1, 5, 9, 2, 6] 30
-- Yes
--
solve :: [Int] -> Int -> IO ()
solve as x
  | amt <= x = putStrLn "Yes"
  | otherwise = putStrLn "No"
  where
    amt = sum $ zipWith (-) as (cycle [0, 1])

main :: IO ()
main = do
  [n, x] <- map (fst . fromJust . BS.readInt) <$> BS.words <$> BS.getLine
  as <- map (fst . fromJust . BS.readInt) <$> BS.words <$> BS.getLine
  solve as x

