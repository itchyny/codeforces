import Data.Char (isLower, isUpper, toLower, toUpper)

main :: IO ()
main = getLine >>= putStrLn . solve

solve :: String -> String
solve cs = map (if length (filter isLower cs) >= length (filter isUpper cs) then toLower else toUpper) cs
