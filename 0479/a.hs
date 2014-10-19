main :: IO ()
main = getContents >>= print . solve . map read . lines

solve :: [Int] -> Int
solve [a, b, c] = maximum [ a + b + c, a + b * c, a * b + c, a * b * c, (a + b) * c, a * (b + c) ]
solve _ = undefined
