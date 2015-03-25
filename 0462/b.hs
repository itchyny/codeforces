import Data.List (genericLength, group, sort, sortBy)

main :: IO ()
main = getLine >>= \s -> getLine >>= print . solve (map read (words s) !! 1)

solve :: Integer -> String -> Integer
solve k = solve' k . sortBy (flip compare) . map genericLength . group . sort

solve' :: Integer -> [Integer] -> Integer
solve' _ [] = 0
solve' m (x:xs) | m <= x = m * m
                | otherwise = x * x + solve' (m - x) xs
