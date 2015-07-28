main :: IO ()
main = getContents >>= putStrLn . unwords . map show . solve . map read . words

solve :: [Int] -> [Int]
solve [ y, k, n ] = head $ filter (not . null) $ [k - y `mod` k, 2 * k - y `mod` k .. n - y] : [[-1]]
solve _ = undefined
