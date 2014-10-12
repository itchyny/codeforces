main :: IO ()
main = getLine >>= print . solve . map read . words

-- sum [ x | q <- [ 1 .. b - 1 ], k <- [ 1 .. a ], let p = q * k, let x = p * b + q ]
solve :: [Integer] -> Integer
solve [a, b] = ((a + a * (a + 1) * b `div` 2) * (b * (b - 1) `div` 2)) `mod` 1000000007
solve _ = undefined
