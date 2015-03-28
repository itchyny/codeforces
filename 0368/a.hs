import Data.List (sort)

main :: IO ()
main = getContents >>= print . solve . map (map read . words) . lines

solve :: [[Int]] -> Int
solve [[_, d], as, [m]] = sum $ take m $ sort as ++ repeat (-d)
solve _ = undefined
