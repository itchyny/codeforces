main :: IO ()
main = getLine >>= putStrLn . unwords . map show . solve . map read . words

solve :: [Int] -> [Int]
solve [ x1, y1, x2, y2 ] | x1 == x2 = [ x1 + y1 - y2, y1, x2 + y1 - y2, y2 ]
                         | y1 == y2 = [ x1, y1 + x1 - x2, x2, y2 + x1 - x2 ]
                         | abs (x1 - x2) == abs (y1 - y2) = [ x1, y2, x2, y1 ]
                         | otherwise = [ -1 ]
solve _ = undefined
