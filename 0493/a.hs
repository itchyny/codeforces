import Data.Function (on)
import Data.List (foldl', sortBy)
import qualified Data.Map as M

main :: IO ()
main = getContents >>= mapM_ putStrLn . solve . lines

solve :: [String] -> [String]
solve (home:away:_:xs) = map format $ sortBy (compare `on` snd) $ M.toList
                       $ M.filter isRedCard $ foldl' go M.empty xs
  where go m x = M.insertWith' f (team, number) (card, read time :: Integer) m
          where [time, team, number, card] = words x
                f (_, t) ("y", _) = ("r", t); f _ p@("r", _) = p; f n _ = n
        format ((team, number), (_, time)) = unwords [ teamName team, number, show time ]
        teamName "h" = home; teamName _ = away
        isRedCard = (=="r") . fst
solve _ = undefined
