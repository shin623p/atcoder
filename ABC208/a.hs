import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

-- | ABC209 D
--
-- >>> solve [2, 11]
-- "Yes"
--
-- >>> solve [2, 13]
-- "No"
--
-- >>> solve [100, 600]
-- "Yes"
--
solve :: [Int] -> String
solve [a, b]
  | b < a     = "No"
  | b > a * 6 = "No"
  | otherwise = "Yes"

main :: IO ()
main = do
  s <- getLine
  putStrLn $ solve $ map read $ words s
