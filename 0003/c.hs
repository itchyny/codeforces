import Data.List (transpose)

main :: IO ()
main = getContents >>= putStrLn . solve . lines

solve :: [String] -> String
solve xs
  | c1 == c2 && w1 = "illegal"
  | c1 == c2 && w2 = "the second player won"
  | c1 == c2 = "first"
  | c1 == c2 + 1 && w1 = "the first player won"
  | c1 == c2 + 1 && w2 = "illegal"
  | c1 == c2 + 1 && islast = "draw"
  | c1 == c2 + 1 = "second"
  | otherwise = "illegal"
  where count c = length (filter (==c) (concat xs))
        islast = count '.' == 0
        win c = any (all (==c)) xs || any (all (==c)) (transpose xs)
             || all (==c) [ xs !! i !! j | (i, j) <- [ (0, 0), (1, 1), (2, 2) ] ]
             || all (==c) [ xs !! i !! j | (i, j) <- [ (2, 0), (1, 1), (0, 2) ] ]
        c1 = count 'X'; c2 = count '0'
        w1 = win 'X'; w2 = win '0'
