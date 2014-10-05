main :: IO ()
main = getLine >>= putStrLn . solve

solve :: String -> String
solve cs = if any (`elem` "HQ9") cs then "YES" else "NO"
