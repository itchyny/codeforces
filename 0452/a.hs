main :: IO ()
main = getLine >> getLine >>= putStrLn . solve

solve :: String -> String
solve cs = head [ ans | ans <- answers, ans =~# cs ]

answers :: [String]
answers = ["vaporeon", "jolteon", "flareon", "espeon", "umbreon", "leafeon", "glaceon", "sylveon"]

(=~#) :: String -> String -> Bool
(x:xs) =~# (y:ys) = (x == y || y == '.') && xs =~# ys
"" =~# "" = True
_ =~# _ = False
