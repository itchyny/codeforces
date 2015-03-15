main :: IO ()
main = readLn >>= print . solve

solve :: Integer -> Integer
solve n | n >= 0 = n
        | otherwise = max (read (init n')) (read (init (init n') ++ [last n']))
          where n' = show n
