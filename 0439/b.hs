import Data.List (sort)

main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Integer] -> Integer
solve (_:x:cs) = sum $ zipWith (*) (sort cs) (map (max 1) [x, x-1..])
solve _ = undefined
