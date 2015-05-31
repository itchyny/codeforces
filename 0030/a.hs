import Data.Maybe (listToMaybe)

main :: IO ()
main = getContents >>= putStrLn . maybe "No solution" show . (\[ a, b, x ] -> solve a b x) . map read . words

solve :: Integer -> Integer -> Integer -> Maybe Integer
solve a b n = listToMaybe [ x | x <- [-1000..1000], a * x ^ n == b ]
