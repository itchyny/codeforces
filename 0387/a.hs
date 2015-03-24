import Text.Printf (printf)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

main :: IO ()
main = getContents >>= print . solve . lines

solve :: [String] -> Time
solve [s, t] = read s -* read t
solve _ = undefined

data Time = Time Int Int

instance Show Time where
  show (Time h m) = printf "%02d:%02d" h m

instance Read Time where
  readsPrec _ s = [(Time (read h) (read m), "")]
    where (h, _:m) = splitAt (fromJust (elemIndex ':' s)) s

(-*) :: Time -> Time -> Time
Time hx mx -* Time hy my
  | mx < my = Time (hx - 1) (mx + 60) -* Time hy my
  | hx < hy = Time (hx + 24) mx -* Time hy my
  | otherwise = Time (hx - hy) (mx - my)
