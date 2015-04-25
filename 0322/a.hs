main :: IO ()
main = getContents >>= (\[n, m] -> (print (n + m - 1) >> mapM_ (putStrLn . unwords . map show) (solve n m))) . map read . words

solve :: Integer -> Integer -> [[Integer]]
solve n m = [ [ 1, k ] | k <- [1..m] ] ++ [ [ k, 1 ] | k <- [2..n] ]
