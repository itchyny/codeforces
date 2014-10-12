main :: IO ()
main = getContents >>= print . solve . map (map read . words) . lines

solve :: [[Int]] -> Int
solve [[n], as] = maximum [ sum (fliprange i j as) | i <- [0..n-1], j <- [i..n-1] ]
solve _ = undefined

fliprange :: Int -> Int -> [Int] -> [Int]
fliprange i j = zipWith ($) [ if i <= k && k <= j then (1-) else id | k <- [0..] ]
