main :: IO ()
main = getContents >>= print . solve . map (map read . words) . lines

solve :: [[Integer]] -> Integer
solve ([_, s] : xys) = maximum $ -1 : [ if y > 0 then 100 - y else 0 | [ x, y ] <- xys, x * 100 + y <= s * 100 ]
solve _ = undefined
