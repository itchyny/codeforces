import Data.Char (ord)

main :: IO ()
main = getLine >>= \s -> readLn >>= \k -> getContents >>= print . solve s k . map read . words

solve :: String -> Int -> [Int] -> Int
solve s k ws = sum $ zipWith (*) [1..] (map (\c -> ws !! (ord c - ord 'a')) s ++ replicate k (maximum ws))
