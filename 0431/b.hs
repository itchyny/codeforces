import Control.Arrow ((&&&))
import Data.List (permutations)
import Data.Tuple (swap)

main :: IO ()
main = getContents >>= print . solve . map (map read . words) . lines

solve :: [[Int]] -> Int
solve gs = maximum [ sum [ gs !! i !! j | (i, j) <- indices is ] | is <- permutations [0..4] ]

indices :: [a] -> [(a, a)]
indices = uncurry (++) . (id &&& map swap) . concat . takeWhile (not . null)
        . iterate (drop 2) . uncurry zip . (id &&& tail)
