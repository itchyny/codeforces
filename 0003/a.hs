import Data.Char (ord)
import Data.List (unfoldr)

main :: IO ()
main = getContents >>= f . solve . words
  where f xs = print (length xs) >> mapM_ putStrLn xs

solve :: [String] -> [String]
solve xs = unfoldr f (tx - sx, ty - sy)
  where [ [ sx, sy ], [ tx, ty ] ] = map (map ord) xs
        f (0, 0) = Nothing
        f (dx, dy) = Just ([ "L", "", "R" ] !! (signum dx + 1)
                        ++ [ "D", "", "U" ] !! (signum dy + 1),
                           (dx - signum dx, dy - signum dy))
