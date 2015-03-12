main :: IO ()
main = getLine >>= print . solve . map read . words

data Player = First | Second deriving Show

solve :: [Int] -> Player
solve (n1:n2:_) = if n1 > n2 then First else Second
solve _ = undefined
