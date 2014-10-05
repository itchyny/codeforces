import Data.Char (toLower)

main :: IO ()
main = getContents >>= print . solve . map (map toLower) . lines

solve :: [String] -> Int
solve [x, y] = case x `compare` y of LT -> -1; EQ -> 0; GT -> 1
solve _ = undefined
