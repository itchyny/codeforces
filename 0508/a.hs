import qualified Data.Set as S

main :: IO ()
main = getContents >>= print . solve . map (map read . words) . tail . lines

solve :: [[Int]] -> Int
solve = go 1 S.empty
  where go t s ([i, j] : ij) | any (all (`S.member`s)) ij' = t
                             | otherwise = go (t + 1) (S.insert (i, j) s) ij
           where ij' = [ [ (i - 1, j), (i - 1, j - 1), (i, j - 1) ] ,
                         [ (i - 1, j), (i - 1, j + 1), (i, j + 1) ] ,
                         [ (i + 1, j), (i + 1, j - 1), (i, j - 1) ] ,
                         [ (i + 1, j), (i + 1, j + 1), (i, j + 1) ] ]
        go _ _ _ = 0
