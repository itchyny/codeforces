import Data.List (sort)

main :: IO ()
main = getContents >>= putStrLn . unwords . map show . solve . map read . words

solve :: [Int] -> [Int]
solve (n:k:as) | k <= n = [ sort as !! (n - k), 0 ]
               | otherwise = [ -1 ]
solve _ = undefined
