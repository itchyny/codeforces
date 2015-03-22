main :: IO ()
main = getContents >>= mapM_ (putStrLn . unwords . map show) . solve . map (map read . words) . lines

solve :: [[Int]] -> [[Int]]
solve ([_, v] : kss) = [ [ length is ], is ]
  where is = [ i | (i, _:ss) <- zip [1..] kss, any (<v) ss ]
solve _ = undefined
