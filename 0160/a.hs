import Data.List (sort)

main :: IO ()
main = getContents >>= print . solve . map read . words . (!!1) . lines

solve :: [Int] -> Int
solve as = length as - length (takeWhile (<= ((sum as - 1) `div` 2)) (scanl1 (+) (sort as)))
