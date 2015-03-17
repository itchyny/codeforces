main :: IO ()
main = getContents >>= print . solve . map (map read . words) . lines

solve :: [[Int]] -> Int
solve [[_, x], xs] = (abs (sum xs) + x - 1) `div` x
solve _ = undefined
