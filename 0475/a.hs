main :: IO ()
main = readLn >>= mapM_ putStrLn . solve

solve :: Int -> [String]
solve k = [ "+------------------------+" ]
       ++ [ '|' : concatMap (:".") [ f i j | j <- [0..10::Int] ] ++ g i | i <- [0..3::Int] ]
       ++ [ "+------------------------+" ]
  where f 0 i | 3 * i + 2 <= k || i == 0 && 1 <= k = 'O' | otherwise = '#'
        f 1 i | 3 * i + 3 <= k || i == 0 && 2 <= k = 'O' | otherwise = '#'
        f 2 0 | 3 <= k                             = 'O' | otherwise = '#'
        f 2 _ = '.'
        f _ i | 3 * i + 4 <= k                     = 'O' | otherwise = '#'
        g 0 = "|D|)"
        g 1 = "|.|"
        g 2 = "..|"
        g _ = "|.|)"
