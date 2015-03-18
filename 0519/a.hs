import Data.Char (isLower, isUpper)

main :: IO ()
main = getContents >>= putStrLn . solve

solve :: String -> String
solve cs = case compare (sum (map score (filter isUpper cs)))
                        (sum (map score (filter isLower cs))) of
                LT -> "Black"
                GT -> "White"
                EQ -> "Draw"

score :: Char -> Int
score 'q' = 9
score 'Q' = 9
score 'r' = 5
score 'R' = 5
score 'b' = 3
score 'B' = 3
score 'n' = 3
score 'N' = 3
score 'p' = 1
score 'P' = 1
score  _  = 0
