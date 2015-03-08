main :: IO ()
main = getContents >>= putStrLn . yesno . solve . map (map read . words) . lines

solve :: [[Int]] -> Bool
solve [as, bs, [n]] = sum as // 5 + sum bs // 10 <= n
solve _ = undefined

(//) :: Integral a => a -> a -> a
x // y = (x + y - 1) `div` y

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
