main :: IO ()
main = getContents >>= mapM_ print . solve . map (map read . words) . tail . lines

solve :: [[Integer]] -> [Integer]
solve [ as, bs, cs ] = [ sum as - sum bs, sum bs - sum cs ]
solve _ = undefined
