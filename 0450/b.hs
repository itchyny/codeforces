main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Int] -> Int
solve [ x, y, n ] = [ x, y, y - x, - x, - y, x - y ] !! ((n - 1) `mod` 6) `mod` 1000000007
solve _ = undefined
