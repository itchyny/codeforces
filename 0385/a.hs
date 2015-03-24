main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Integer] -> Integer
solve (_:c:xs) = max 0 $ maximum (zipWith (-) xs (tail xs)) - c
solve _ = undefined
