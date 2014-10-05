import Data.Char
import Data.List

main :: IO ()
main = getContents >>= mapM_ (print . convert . read) . tail . lines

data Cell = Ax String Integer | Rxcy Integer Integer

instance Show Cell where
  show (Ax a x) = a ++ show x
  show (Rxcy x y) = "R" ++ show x ++ "C" ++ show y

instance Read Cell where
  readsPrec _ s = [(listToCell list, "")]
    where list = foldl go [] s
          go [] c = [[c]]
          go xs c | isAlpha (head (head xs)) == isAlpha c = (head xs++[c]):tail xs
                  | otherwise = [c]:xs
          listToCell [y, "C", x, "R"] = Rxcy (read x) (read y)
          listToCell [x, a] = Ax a (read x)
          listToCell _ = error "Input error"

convert :: Cell -> Cell
convert (Ax a x) = Rxcy x idx
  where nums = map (\c -> fromIntegral $ ord c - ord 'A' + 1) (reverse a)
        idx = sum $ map (\(b, y) -> b * 26 ^ (y :: Integer)) $ zip nums [0..]
convert (Rxcy x y) = Ax str x
  where str = reverse $ unfoldr go y
        go 0 = Nothing
        go n = Just (chr (ord 'A' + fromIntegral m), d)
          where (d, m) = divMod (n - 1) 26
