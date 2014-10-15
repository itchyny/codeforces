import Data.List (group)

main :: IO ()
main = getContents >>= print . length . group . tail . lines
