main :: IO ()
main = getContents >>= print . solve . toTuple . map (map read . words) . tail . lines

solve :: ([Integer], [Integer]) -> Integer
solve (as, bs) = head $ [ v | v <- [ maximum as .. minimum bs - 1 ], 2 * minimum as <= v ] ++ [-1]

toTuple :: [a] -> (a, a)
toTuple xs = (head xs, xs !! 1)
