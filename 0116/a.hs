main :: IO ()
main = getContents >>= print . solve . map (map read . words) . tail . lines

solve :: [[Int]] -> Int
solve = maximum . scanl (+) 0 . map (\[a, b] -> b - a)
