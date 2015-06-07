import Data.List (sort)

main :: IO ()
main = getLine >>= putStrLn . drop 4 . solve (-1) (-1) . sort . read . ('[':) . (++"]")

solve :: Int -> Int -> [Int] -> String
solve k l [] | k < l = ',' : show k ++ '-' : show l
             | otherwise = ',' : show k
solve k l (x:xs) | l + 1 < x = solve k l [] ++ solve x x xs
                 | otherwise = solve k x xs
