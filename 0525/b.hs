import Data.List (group, sort)
import Control.Arrow (first, second)

main :: IO ()
main = getLine >>= \s -> getLine >> getLine >>= putStrLn . solve s . map read . words

solve :: String -> [Int] -> String
solve s as = uncurry (++) $ first reverse $ go True ("", "") cssdss [1..] bs
  where go b (ps, qs) (c:cs, d:ds) (i:is) xxs@(x:xs)
          | i < x && b = go      b  (c:ps, d:qs) (cs, ds) is xxs
          | i < x      = go      b  (d:ps, c:qs) (cs, ds) is xxs
          | b          = go (not b) (d:ps, c:qs) (cs, ds) is xs
          | otherwise  = go (not b) (c:ps, d:qs) (cs, ds) is xs
        go b (ps, qs) (ccs, dds) _ _
          | b          = (reverse ccs ++ ps, reverse dds ++ qs)
          | otherwise  = (reverse dds ++ ps, reverse ccs ++ qs)
        bs = map head $ filter (odd . length) $ group $ sort as
        cssdss = second reverse $ splitAt (length s `div` 2) s
