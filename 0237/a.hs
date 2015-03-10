import Data.List (group, sort)

main :: IO ()
main = getContents >>= print . maximum . map length . group . sort . tail . lines
