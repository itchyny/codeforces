main :: IO ()
main = getContents >>= putStrLn . unwords . map show . solve . map read . words

solve :: [Integer] -> [Integer]
solve [ l, r ] | a + 2 <= r = [ a, a + 1, a + 2 ]
               | otherwise = [ -1 ]
  where a = (max 2 l + 1) `div` 2 * 2
solve _ = undefined
