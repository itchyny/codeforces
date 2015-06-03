import Data.List (sort)

main :: IO ()
main = getContents >>= print . solve . map read . tail . words

solve :: [Int] -> Int
solve (m:as) = negate $ sum $ take m $ sort $ map (min 0) as
solve _ = undefined
