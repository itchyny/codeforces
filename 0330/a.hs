import Data.List (transpose)

main :: IO ()
main = getContents >>= print . solve . tail . lines

solve :: [String] -> Int
solve = length . filter (=='_') . concat . eat . transpose . eat
  where eat = map (\cs -> if all (`elem`"._") cs then map (const '_') cs else cs)
