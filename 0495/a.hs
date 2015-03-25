import Data.Char (digitToInt)

main :: IO ()
main = getLine >>= print . solve

solve :: String -> Integer
solve = product . map ((!!) [2, 7, 2, 3, 3, 4, 2, 5, 1, 2] . digitToInt)
