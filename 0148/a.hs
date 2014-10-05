main :: IO ()
main = getContents >>= print . solve . map read . lines

solve :: [Int] -> Int
solve [k, l, m, n, d] = length [ x | x <- [1..d], any ((==0) . (x`mod`)) [k, l, m, n] ]
solve _ = undefined
