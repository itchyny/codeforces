import Data.Function (on)
import Data.Char (digitToInt)

main :: IO ()
main = getContents >>= putStrLn . yesno . solve . lines

solve :: [String] -> Bool
solve [n, cs] = all (`elem`"47") cs && uncurry ((==) `on` (sum . map digitToInt)) (splitAt (read n `div` 2) cs)
solve _ = undefined

yesno :: Bool -> String
yesno True = "YES"
yesno _ = "NO"
