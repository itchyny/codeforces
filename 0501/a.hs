import Data.Function (on)

main :: IO ()
main = getContents >>= putStrLn . solve . map read . words

solve :: [Integer] -> String
solve [ a, b, c, d ] = case (compare `on` score) (a, c) (b, d) of
                            GT -> "Misha"; LT -> "Vasya"; _ -> "Tie"
solve _ = undefined

score :: (Integer, Integer) -> Integer
score (p, t) = max (3 * p `div` 10) (p - p `div` 250 * t)
