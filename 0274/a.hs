import Control.Applicative ((<$>), (<*>))
import Data.List (foldl', sort)
import qualified Data.Set as S

main :: IO ()
main = print =<< solve <$> (read <$> (!!1) <$> words <$> getLine) <*> (map read <$> words <$> getLine)

solve :: Integer -> [Integer] -> Int
solve k = S.size . foldl' (\s x -> if S.member x s then s else S.insert (k * x) s) S.empty . sort
