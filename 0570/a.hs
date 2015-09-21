import Control.Arrow ((&&&))
import Data.List (elemIndex, group, sort)
import Data.Maybe (fromJust)

main :: IO ()
main = getContents >>= print . solve . map (map read . words) . tail . lines

solve :: [[Int]] -> Int
solve = succ . negate . snd . maximum . map (length &&& negate . head) . group . sort . map maximumIndex

maximumIndex :: Ord a => [a] -> Int
maximumIndex xs = fromJust $ elemIndex (maximum xs) xs
