import Data.List (isPrefixOf)

main :: IO ()
main = getContents >>= mapM_ putStrLn . solve [] . tail . lines

solve :: [String] -> [String] -> [String]
solve path (c:cs) | c == "pwd" = (concatMap ('/':) path ++ "/") : solve path cs
                  | "cd " `isPrefixOf` c = solve (cd (splitOn '/' (dropWhile (==' ') (drop 3 c))) path) cs
                  | otherwise = solve path cs
  where cd ("..":ds) ps = cd ds (init ps)
        cd ("":ds) _ = cd ds []
        cd (d:ds) ps = cd ds (ps ++ [d])
        cd [] ps = ps
solve _ _ = []

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c xxs@(x:xs)
  | x == c = [] : splitOn c xs
  | otherwise = takeWhile (/=c) xxs : splitOn c (dropOne (==c) $ dropWhile (/=c) xxs)
    where dropOne f yys@(y:ys) | f y = ys | otherwise = yys
          dropOne _ [] = []
splitOn _ [] = []
