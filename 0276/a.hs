main :: IO ()
main = getContents >>= print . solve . map (map read . words) . lines

solve :: [[Integer]] -> Integer
solve ([_, k] : fts) = maximum [ f - max 0 (t - k) | [ f, t ] <- fts ]
solve _ = undefined
