import qualified Data.Array as A
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S
import Data.Maybe (fromJust)

main :: IO ()
main = B.getContents >>= mapM_ print . solve . map (fst . fromJust . B.readInt) . B.words

solve :: [Int] -> [Int]
solve (n:_:asls) = [ S.size $ aset A.! l | l <- ls ]
  where (as, ls) = splitAt n asls
        aset = A.listArray (1, n) $ scanr S.insert S.empty as
solve _ = undefined
