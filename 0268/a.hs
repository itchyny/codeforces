main :: IO ()
main = getContents >>= print . solve . map (map read . words) . lines

solve :: [[Int]] -> Int
solve ([n] : has) = length [ () | i <- [0..n-1], j <- [0..n-1], i /= j, hs !! i == as !! j ]
  where hs = map head has
        as = map (!!1) has
solve _ = undefined
