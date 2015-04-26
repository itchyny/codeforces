main :: IO ()
main = getContents >>= print . solve . map (map read . words) . lines

solve :: [[Int]] -> Int
solve [_, ds, [a, b]] = sum $ drop (a - 1) $ take (b - 1) ds
solve _ = undefined
