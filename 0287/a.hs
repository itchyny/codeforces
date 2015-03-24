main :: IO ()
main = getContents >>= putStrLn . yesno . solve . lines

solve :: [String] -> Bool
solve cs = or [ length [ () | i' <- [i..i+1], j' <- [j..j+1], cs !! i' !! j' == '#' ] /= 2 | i <- [0..2], j <- [0..2] ]

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
