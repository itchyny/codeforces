import Data.List (sort)

main :: IO ()
main = getContents >>= print . solve . map read . words . (!!1) . lines

solve :: [Integer] -> Integer
solve as = sum (zipWith (*) [1..] (sort as)) - maximum as + sum as
