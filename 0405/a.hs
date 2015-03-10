main :: IO ()
main = getContents >>= putStrLn . unwords . map show . solve . solve . map read . words . (!!1) . lines

solve :: [Int] -> [Int]
solve ns = [ length (filter (>=k) ns) | k <- [ maximum ns, maximum ns - 1 .. 1 ] ]
