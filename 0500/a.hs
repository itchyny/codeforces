main :: IO ()
main = getContents >>= putStrLn . yesno . solve . map (map read . words) . lines

solve :: [[Int]] -> Bool
solve [[_, t], as] = go t as
  where go 1 _ = True
        go n _ | n < 1 = False
        go n xs@(x:_) = go (n - x) (drop x xs)
        go _ _ = False
solve _ = undefined

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
