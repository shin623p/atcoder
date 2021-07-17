module A where
-- | ABC208 A
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
