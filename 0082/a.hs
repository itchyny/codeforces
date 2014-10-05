main :: IO ()
main = readLn >>= putStrLn . solve

solve :: Int -> String
solve = go 1 . pred
  where names = [ "Sheldon", "Leonard", "Penny", "Rajesh", "Howard" ]
        go k m | m < 5 * k = names !! (m `div` k)
               | otherwise = go (2 * k) (m - 5 * k)
