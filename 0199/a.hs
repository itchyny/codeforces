main :: IO ()
main = getLine >>= putStrLn . ("0 0 "++)
