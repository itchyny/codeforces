import Data.List (group, sort)

main :: IO ()
main = getContents >>= print . solve . map read . tail . words

solve :: [Int] -> Int
solve = solve' . map length . group . sort . filter (>0)

solve' :: [Int] -> Int
solve' xs | any (>2) xs = -1
          | otherwise = length $ filter (==2) xs
