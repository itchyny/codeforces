import Data.List (group, sort)

main :: IO ()
main = readLn >>= \k -> getLine >>= putStrLn . solve k

solve :: Int -> String -> String
solve k = maybe "-1" (([1..k] >>) . concat) . mapM (\xs@(x:_) -> case divMod (length xs) k of (d, 0) -> Just (replicate d x); _ -> Nothing) . group . sort
