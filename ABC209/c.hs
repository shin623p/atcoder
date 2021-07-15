import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
-- import  Control.Monad

-- | ABC209 C
--
-- >>> solve [1, 3]
-- 2
--
-- >>> solve [3, 3, 4, 4]
-- 12
--
-- >>> solve [999999917, 999999914, 999999923, 999999985, 999999907, 999999965, 999999914, 999999908, 999999951, 999999979]
-- 405924645
--
solve :: [Int] -> Int
solve cs = foldr (\(x, y) z -> (z * (y - x)) `mod` (10^9 + 7)) 1 $ zip [0..] $ sort cs

main :: IO ()
main = do
  _ <- BS.getLine
  cs <- map (fst . fromJust . BS.readInt) <$> BS.words <$> BS.getLine
  print $ solve cs

