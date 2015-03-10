import Data.Char (isAlpha)
import Data.List (group, sort)

main :: IO ()
main = getLine >>= print . length . group . sort . filter isAlpha
