import Data.List (sortBy)

main :: IO ()
main = getContents >>= print . solve . map (map read . words) . lines

solve :: [[Int]] -> Int
solve [[_, k], fs] = (2*) $ sum [ f | (i, f) <- zip [0..] $ map (subtract 1) $ sortBy (flip compare) fs, i `mod` k == 0 ]
solve _ = undefined
