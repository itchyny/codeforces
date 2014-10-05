import Data.List (sort)
import Data.Char (isDigit)

main :: IO ()
main = getLine >>= putStrLn . tail . foldr (\c s -> '+' : c : s) "" . sort . filter isDigit
