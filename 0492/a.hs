main :: IO ()
main = readLn >>= print . solve

solve :: Int -> Int
solve n = head [ k - 1 | k <- [1..], n < k * (k + 1) * (k + 2) `div` 6 ]
