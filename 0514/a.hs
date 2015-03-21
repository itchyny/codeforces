import Data.Char (digitToInt, intToDigit)

main :: IO ()
main = getLine >>= putStrLn . solve

solve :: String -> String
solve ('9':cs) = '9' : solve' cs
solve cs = solve' cs

solve' :: String -> String
solve' cs = [ if c < '5' then c else intToDigit (9 - digitToInt c) | c <- cs ]
