import Data.Char (isLower, isUpper, toLower, toUpper)

main :: IO ()
main = getLine >>= putStrLn . solve

solve :: String -> String
solve ccs@(c:cs) | all isUpper ccs = map toLower ccs
                 | isLower c && all isUpper cs = toUpper c : map toLower cs
                 | otherwise = ccs
solve "" = ""
