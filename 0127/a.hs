import Control.Applicative ((<$>), (<*>))

main :: IO ()
main = print =<< (*) <$> ((!!1) . map read . words <$> getLine) <*> (solve <$> (map (map read . words) . lines <$> getContents))

solve :: [[Double]] -> Double
solve xys = sum [ sqrt ((x - x') ** 2 + (y - y') ** 2) | ([x, y], [x', y']) <- zip xys (tail xys) ] / 50
