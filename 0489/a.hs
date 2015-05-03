import Data.List (elemIndex)
import Data.Maybe (fromJust)

main :: IO ()
main = getContents >>= printSolution . solve 0 . map read . tail . words

solve :: Int -> [Int] -> [[Int]]
solve _ [] = []
solve i aas@(a:as) = (if j > 0 then ([i, i + j]:) else id) (solve (i + 1) [ if j == k then a else b | (k, b) <- zip [1..] as ])
  where j = fromJust $ elemIndex (minimum aas) aas

printSolution :: Show a => [[a]] -> IO ()
printSolution xs = print (length xs) >> mapM_ (putStrLn . unwords . map show) xs
