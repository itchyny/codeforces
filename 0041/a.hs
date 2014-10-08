main :: IO ()
main = getContents >>= putStrLn . solve . lines

solve :: [String] -> String
solve [s, t] = yesno $ reverse s == t
solve _ = undefined

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
