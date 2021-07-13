solve :: Int -> Int -> Int
solve a b
  | a > b = 0
  | otherwise = b - a + 1

main :: IO ()
main = do
  s <- getLine
  print $ (\[a, b] -> solve a b ) $ map read $ words s
