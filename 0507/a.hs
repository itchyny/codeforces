import Data.List (sort)

main :: IO ()
main = getContents >>= mapM_ (putStrLn . unwords . map show) . solve . map (map read . words) . lines

solve :: [[Int]] -> [[Int]]
solve [[_, k], as] = g $ filter ((<=k) . fst) (scanl f (0, 0) (sort (zip as [1..])))
  where f (a, _) (c, i) = (a + c, i)
        g xs = [ [ length xs - 1 ], map snd (tail xs) ]
solve _ = undefined
