main :: IO ()
main = getContents >>= print . solve . lines

solve :: [String] -> Int
solve [_, cs] = min (length cs) (length (takeWhile (=='1') cs) + 1)
solve _ = undefined
