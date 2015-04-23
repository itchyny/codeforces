import Data.List (group, transpose)

main :: IO ()
main = getContents >>= print . solve . map (map read . words) . tail . lines

solve :: [[Int]] -> Int
solve xys = case (map (even . sum) (transpose xys), length (filter ((>1) . length . group . map even) xys ) > 1) of
                 ([ True, True ], _) -> 0
                 ([ False, False ], True) -> 1
                 _ -> -1
