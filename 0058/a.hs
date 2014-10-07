main :: IO ()
main = getLine >>= putStrLn . solve

solve :: String -> String
solve = yesno . isContainedIn "hello"

isContainedIn :: Eq a => [a] -> [a] -> Bool
isContainedIn xxs@(x:xs) (y:ys) | x == y = isContainedIn xs ys
                                | otherwise = isContainedIn xxs ys
isContainedIn [] _ = True
isContainedIn _ _ = False

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
