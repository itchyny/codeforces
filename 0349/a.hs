main :: IO ()
main = getContents >>= putStrLn . yesno . solve . map read . words . (!!1) . lines

solve :: [Int] -> Bool
solve = all g . scanl f (0 :: Int, 0 :: Int)
  where f (a, b) 25 = (a + 1, b)
        f (a, b) 50 = (a - 1, b + 1)
        f (a, b) 100 | b > 0 = (a - 1, b - 1)
                     | otherwise = (a - 3, b)
        f ab _ = ab
        g (a, b) = a >= 0 && b >= 0

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
