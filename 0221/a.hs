main :: IO ()
main = readLn >>= putStrLn . unwords . map show . solve

solve :: Integer -> [Integer]
solve n = n : [1..n-1]
