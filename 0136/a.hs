import Data.List (elemIndex)
import Data.Maybe (fromJust)

main :: IO ()
main = getContents >>= putStrLn . format . solve . map (map read . words) . lines

solve :: [[Int]] -> [Int]
solve [[n], ps] = [ 1 + fromJust (elemIndex i ps) | i <- [1..n] ]
solve _ = undefined

format :: [Int] -> String
format = unwords . map show
