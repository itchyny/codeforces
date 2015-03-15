import Data.List (foldl')

main :: IO ()
main = getContents >>= print . solve . map (map read . words) . lines

solve :: [[Integer]] -> Integer
solve [[n, _], as] = snd $ foldl' (\(a, b) c -> (c, b + (c + n - a) `mod` n)) (1, 0) as
solve _ = undefined
