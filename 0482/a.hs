main :: IO ()
main = getContents >>= putStrLn . unwords . map show . solve . map read . words

solve :: [Integer] -> [Integer]
solve [ n, k ] = [ 1 .. n - k ] ++ merge [ n, n - 1 .. n - k `div` 2 + 1 ] [ n - k + 1 .. n - k `div` 2 ]
solve _ = undefined

merge :: [a] -> [a] -> [a]
merge (x:xs) (y:ys) = x : y : merge xs ys
merge xs [] = xs
merge _ ys = ys
