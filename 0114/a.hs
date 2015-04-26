import Control.Applicative ((<$>), (<*>))
import Data.Maybe (listToMaybe)

main :: IO ()
main = mapM_ putStrLn . maybe ["NO"] (\n -> ["YES", show n]) =<< solve <$> readLn <*> readLn

solve :: Integer -> Integer -> Maybe Integer
solve k l = listToMaybe [ m - 1 | k ^ m == l ]
  where m = round (logBase (fromIntegral k) (fromIntegral l) :: Double)
