import Data.List (sortBy)

main :: IO ()
main = getContents >>= print . solve . sortBy (flip compare) . map read . tail . words

solve :: [Integer] -> Integer
solve (a:b:c:d:xs) | a <= b + 1 && c <= d + 1 = b * d + solve xs
                   | a <= b + 1 = solve (a:b:d:xs)
                   | otherwise = solve (b:c:d:xs)
solve _ = 0
