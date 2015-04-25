main :: IO ()
main = readLn >>= putStrLn . solve

solve :: Int -> String
solve n | n < 20 = [ "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
                     "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" ] !! n
        | n `mod` 10 == 0 = [ "zero", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety" ] !! (n `div` 10)
        | otherwise = solve (10 * (n `div` 10)) ++ "-" ++ solve (n `mod` 10)
