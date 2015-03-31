main :: IO ()
main = getContents >>= print . (\[a, b] -> solve a b) . map read . words

solve :: Integer -> Integer -> Integer
solve 0 _ = 0
solve a b = b `div` a + solve (b `mod` a) a
