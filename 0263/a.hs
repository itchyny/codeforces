import Control.Arrow (second)
import Data.List (elemIndex)
import Data.Maybe (fromJust, isJust)

main :: IO ()
main = getContents >>= print . solve . map (map read . words) . lines

solve :: [[Int]] -> Int
solve = distance (2, 2) . getIndex 1

getIndex :: Eq a => a -> [[a]] -> (Int, Int)
getIndex n = second fromJust . head . filter (isJust . snd) . zip [0..] . map (elemIndex n)

distance :: Num a => (a, a) -> (a, a) -> a
distance (xa, ya) (xb, yb) = abs (xa - xb) + abs (ya - yb)
