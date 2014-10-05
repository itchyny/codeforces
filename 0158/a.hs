main :: IO ()
main = getContents >>= print . solve . map (map read . words) . lines

solve :: [[Int]] -> Int
solve [[_, k], as'] = go as' 0 (head as')
  where go (a:as) m p
          | a == 0 = m
          | m >= k && a /= p = m
          | otherwise = go as (m + 1) a
        go [] m _ = m
solve _ = 0
