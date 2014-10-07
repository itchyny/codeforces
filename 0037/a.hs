import Data.List (group, sort)

main :: IO ()
main = getContents >>= putStrLn . solve . map read . words . (!!1) . lines

solve :: [Int] -> String
solve ns = unwords $ map show [ maximum (map length gns), length gns ]
  where gns = group (sort ns)
