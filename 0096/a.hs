import Data.List (group)

main :: IO ()
main = getLine >>= putStrLn . solve

solve :: String -> String
solve cs = if any ((>=7) . length) (group cs) then "YES" else "NO"
