main :: IO ()
main = getContents >>= putStrLn . yesno . solve . lines

solve :: [String] -> Bool
solve (ns:cs) = all even [ length [ () | (di, dj) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]
                                       , 0 <= i + di, i + di < n
                                       , 0 <= j + dj, j + dj < n
                                       , cs !! (i + di) !! (j + dj) == 'o' ]
                                       | i <- [0..n-1], j <- [0..n-1] ]
  where n = read ns
solve _ = undefined

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
