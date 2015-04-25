main :: IO ()
main = getContents >>= putStrLn . unwords . map show . solve . map read . words

solve :: [Integer] -> [Integer]
solve (n:_:bs) = [ head [ b | b <- bs, b <= k ] | k <- [1..n] ]
solve _ = undefined
