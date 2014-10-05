import Data.Char (toUpper)

main :: IO ()
main = getLine >>= putStrLn . solve

solve :: String -> String
solve (c:cs) = toUpper c : cs
solve "" = ""
