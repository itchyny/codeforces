main :: IO ()
main = getContents >>= putStrLn . yesno . solve . map read . tail . words

solve :: [Int] -> Bool
solve (s:as) = sum as - maximum as <= s
solve _ = undefined

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
