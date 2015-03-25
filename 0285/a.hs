main :: IO ()
main = getContents >>= putStrLn . unwords . map show . solve . map read . words

solve :: [Integer] -> [Integer]
solve [ n, k ] = [ n, n - 1 .. n - k + 1 ] ++ [ 1 .. n - k ]
solve _ = undefined
