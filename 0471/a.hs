import Data.List (group, sort)

main :: IO ()
main = getLine >>= print . solve . map read . words

data Animal = Bear | Elephant | Alien deriving Show

solve :: [Int] -> Animal
solve ls = case sort (map length (group (sort ls))) of
                [6] -> Elephant
                [2, 4] -> Elephant
                [1, 5] -> Bear
                [1, 1, 4] -> Bear
                _ -> Alien
