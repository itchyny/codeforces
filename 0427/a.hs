main :: IO ()
main = getContents >>= print . solve . map read . words . (!!1) . lines

solve :: [Int] -> Int
solve = sum . map snd . scanl f (0, 0)
  where f (x, _) z = (max (x + z) 0, max (- x - z) 0)
