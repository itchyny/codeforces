import Data.List (foldl', sort)

main :: IO ()
main = getContents >>= print . solve . map (map read . words) . tail . lines

solve :: [[Integer]] -> Integer
solve = foldl' (\x xs -> minimum (filter (>=x) xs)) 0 . sort
