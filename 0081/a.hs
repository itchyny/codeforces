main :: IO ()
main = putStrLn . solve =<< getLine

solve :: String -> String
solve = reverse . foldl go ""
  where
    go (y:ys) x | x == y = ys
    go ys x = x : ys
