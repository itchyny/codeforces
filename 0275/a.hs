main :: IO ()
main = getContents >>= mapM_ (putStrLn . concatMap show) . solve . map (map read . words) . lines

solve :: [[Int]] -> [[Int]]
solve xs = [ [ (1 + sum [ xs !! i' !! j' | j' <- [0..2], i' <- [0..2], abs (i - i') + abs (j - j') < 2 ]) `mod` 2 | j <- [0..2] ] | i <- [0..2] ]
