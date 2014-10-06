import Data.List (nub)

main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Int] -> Int
solve xs = 4 - length (nub xs)
