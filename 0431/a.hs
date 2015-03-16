import Data.Char (digitToInt)

main :: IO ()
main = getContents >>= print . solve . lines

solve :: [String] -> Int
solve [bs, ss] = sum $ map ((as !!) . pred . digitToInt) ss where as = map read (words bs)
solve _ = undefined
