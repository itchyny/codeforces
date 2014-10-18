main :: IO ()
main = getLine >>= putStrLn . unwords . words . solve

solve :: String -> String
solve ('W':'U':'B':cs) = ' ' : solve cs
solve (c:cs) = c : solve cs
solve "" = ""
