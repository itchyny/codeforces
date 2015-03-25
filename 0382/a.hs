main :: IO ()
main = getContents >>= putStrLn . solve . words

solve :: [String] -> String
solve xs | e >= a && e >= b && even d = l ++ take (e - a) s ++ "|" ++ r ++ drop (e - a) s
         | otherwise = "Impossible"
  where l = takeWhile (/='|') (head xs); r = tail $ dropWhile (/='|') (head xs); s = xs !! 1
        a = length l; b = length r; c = length s; d = a + b + c; e = d `div` 2
