import Data.List (group, nub)

main :: IO ()
main = getLine >>= putStrLn . solve

solve :: String -> String
solve = f . odd . length . group . nub
  where f False = "CHAT WITH HER!"
        f _ = "IGNORE HIM!"
