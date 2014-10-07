main :: IO ()
main = getContents >>= print . solve . map read . words . (!!1) . lines

solve :: [Int] -> Int
solve ns = head [ i | (i, n) <- zip [1..] ns, length [ m | m <- ns, even n == even m ] == 1 ]
