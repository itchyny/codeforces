main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Integer] -> Integer
solve [ v1, v2, t, d ] = sum [ min (v1 + d * i) (v2 + d * (t - i - 1)) | i <- [0..t-1] ]
solve _ = undefined
