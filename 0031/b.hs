import Data.Functor ((<$>))
import Data.List (intercalate)

main :: IO ()
main = getLine >>= putStrLn . maybe "No solution" (intercalate ",") . solve

solve :: String -> Maybe [String]
solve xs | null xs = Just []
         | null ys || all (=='@') (take 2 zs) = Nothing
         | '@' `notElem` tail zs = Just [ xs ]
         | otherwise = (:) (ys ++ take 2 zs) <$> solve (drop 2 zs)
  where (ys, zs) = break (=='@') xs
