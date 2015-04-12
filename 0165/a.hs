main :: IO ()
main = getContents >>= print . solve . map (toTuple . map read . words) . tail . lines

solve :: [(Int, Int)] -> Int
solve xys = length [ (x, y) | (x, y) <- xys, all (flip any xys . ($ (x, y))) [ left, right, lower, upper ] ]
  where left (x, y) (x', y') = x' > x && y' == y
        right (x, y) (x', y') = x' < x && y' == y
        lower (x, y) (x', y') = x' == x && y' < y
        upper (x, y) (x', y') = x' == x && y' > y

toTuple :: [a] -> (a, a)
toTuple xs = (head xs, xs !! 1)
