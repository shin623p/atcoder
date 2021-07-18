module Main where


coins = [1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800]

-- | solve
--
-- >>> solve 9
-- 3
--
-- >>> solve 119
-- 10
--
-- >>> solve 10000000
-- 24
--
solve :: Int -> Int
solve p = snd $ foldl (\(p, n) c -> (p `mod` c, (p `div` c) + n)) (p, 0) $ reverse coins


main :: IO ()
main = (print . solve) =<< readLn


