import Data.List (sort)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = B.getContents >>= print . solve . map (fromIntegral . fst . fromJust . B.readInt) . tail . B.words

solve :: [Integer] -> Integer
solve = sum . map abs . zipWith (-) [1..] . sort
