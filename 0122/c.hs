main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Integer] -> Integer
solve [l, r] = sumLucky r - sumLucky (l - 1)
solve _ = undefined

lucky :: [Integer]
lucky = map read $ concat $ tail $ take 11 $ iterate (\xs -> map ('4':) xs ++ map ('7':) xs) [""]

sumLucky :: Integer -> Integer
sumLucky n = sum $ map snd $ scanl f (0, 0) lucky
  where f (x, _) z | z <= n = (z, (z - x) * z)
                   | x >= 0 = (-1, (n - x) * z)
                   | otherwise = (-1, 0)
