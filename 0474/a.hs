import Data.List (elemIndex)
import Data.Maybe (fromJust)

main :: IO ()
main = getContents >>= putStrLn . solve . lines

solve :: [String] -> String
solve [c, cs] | c == "R" = f pred
              | c == "L" = f succ
  where f g = map ((!!) keyboard . g . fromJust . flip elemIndex keyboard) cs
solve _ = undefined

keyboard :: String
keyboard = " qwertyuiop asdfghjkl; zxcvbnm,./ "
