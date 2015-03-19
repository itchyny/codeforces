import Control.Arrow ((***))

main :: IO ()
main = getContents >>= print . solve . map (map read . words) . lines

solve :: [[Int]] -> Int
solve [[n], ds, [s, t]]
  | s == t = 0
  | otherwise = uncurry min $ (sum *** sum) $ splitAt (max s t - min s t)
              $ take n $ drop (min s t - 1) $ cycle ds
solve _ = undefined
