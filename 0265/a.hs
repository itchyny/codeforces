main :: IO ()
main = getContents >>= print . (\[s, t] -> solve (1, s) t) . lines

solve :: (Integer, String) -> String -> Integer
solve = (fst .) . foldl (\(k, s) t -> if head s == t then (k + 1, tail s) else (k, s))
