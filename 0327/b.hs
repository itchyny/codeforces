main :: IO ()
main = readLn >>= putStrLn . unwords . map show . solve

solve :: Integer -> [Integer]
solve n = [n .. 2 * n - 1]
