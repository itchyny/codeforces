import Data.List (sortBy)

main :: IO ()
main = getContents >>= print . solve . map (map read . words) . lines

solve :: [[Int]] -> Int
solve [[k], as] = head ([ i | (i, b) <- zip [0..] (scanl (+) 0 (sortBy (flip compare) as)), b >= k ] ++ [-1])
solve _ = undefined
