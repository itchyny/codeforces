main :: IO ()
main = readLn >>= (\xs -> print (length xs) >> putStrLn (unwords (map show xs))). solve

solve :: Integer -> [Integer]
solve n = (if n > 3 then [2,4..n] else []) ++ [1,3..n]
