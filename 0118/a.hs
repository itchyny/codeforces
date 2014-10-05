import Data.Char (toLower)

main :: IO ()
main = getLine >>= putStrLn . solve

solve :: String -> String
solve cs = concat [ [ '.', c ] | c <- map toLower cs, c `notElem` "aoyeui" ]
