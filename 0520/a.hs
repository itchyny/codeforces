import Data.Char (toLower)

main :: IO ()
main = getContents >>= putStrLn . yesno . solve

solve :: String -> Bool
solve cs = and [ c `elem` map toLower cs | c <- ['a' .. 'z'] ]

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
