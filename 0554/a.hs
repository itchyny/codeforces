main :: IO ()
main = print . solve =<< getLine

solve :: String -> Int
solve xs = 25 * length xs + 26
