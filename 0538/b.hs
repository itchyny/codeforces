import Data.List (unfoldr)
import Data.Maybe (listToMaybe)

main :: IO ()
main = readLn >>= printSolution . solve

solve :: Integer -> [Integer]
solve = unfoldr $ \n -> listToMaybe [ let m = read (map (min '1') (show n)) in (m, n - m) | n > 0 ]

printSolution :: Show a => [a] -> IO ()
printSolution xs = print (length xs) >> putStrLn (unwords (map show xs))
