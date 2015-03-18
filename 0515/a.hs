main :: IO ()
main = getLine >>= putStrLn . yesno . solve . map read . words

solve :: [Int] -> Bool
solve [a, b, s] = abs a + abs b <= s && even (s - abs a - abs b)
solve _ = undefined

yesno :: Bool -> String
yesno True = "Yes"
yesno _ = "No"
