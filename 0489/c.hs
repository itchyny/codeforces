main :: IO ()
main = getContents >>= putStrLn . unwords . map show . solve . map read . words

solve :: [Integer] -> [Integer]
solve [ m, s ]
  | 9 * m < s || m > 1 && s < 1 = [ -1, -1 ]
  | s == 0 = [ 0, 0 ]
  | otherwise = [ 10 ^ (m - 1) + (((s - 1) `mod` 9) + 1) * (10 ^ ((s - 1) `div` 9)) - 1,
                ((10 ^ (s `div` 9) - 1) * 10 + (s `mod` 9)) * (10 ^ (m - (s `div` 9))) `div` 10 ]
solve _ = undefined

