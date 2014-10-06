main :: IO ()
main = getLine >>= putStrLn . solve

solve :: String -> String
solve = f . all (`elem`"47") . show . length . filter (`elem`"47")
  where f True = "YES"; f _ = "NO"
