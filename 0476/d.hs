import Text.Printf (printf)

main :: IO ()
main = getLine >>= printSolution . solve . map read . words

solve :: [Int] -> (Int, [(Int, Int, Int, Int)])
solve [n, k] = (k * (6 * n - 1), [ (k * (6 * i + 1), k * (6 * i + 2), k * (6 * i + 3), k * (6 * i + 5)) | i <- [0..n-1] ])
solve _ = undefined

printSolution :: (Int, [(Int, Int, Int, Int)]) -> IO ()
printSolution (m, ks) = print m >> mapM_ (putStrLn . format) ks
  where format (a, b, c, d) = printf "%d %d %d %d" a b c d
