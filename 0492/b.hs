import Data.List (sort)

main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Double] -> Double
solve (_:l:as) = maximum (head bs : (l - last bs) : map (/2) (zipWith (-) (tail bs) bs))
  where bs = sort as
solve _ = undefined
