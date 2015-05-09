import Data.Char (digitToInt)

main :: IO ()
main = getLine >>= print . solve

solve :: String -> Int
solve = sum . zipWith (*) (iterate (*2) 1) . map ((`div`3) . digitToInt) . reverse
