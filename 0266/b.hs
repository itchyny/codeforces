main :: IO ()
main = getContents >>= putStrLn . solve . (\[nt, gbs] -> ((!!1) $ map read $ words nt, gbs)) . lines

solve :: (Int, String) -> String
solve (t, gbs) = iterate go gbs !! t
  where go "" = ""
        go ('B':'G':xs) = 'G' : 'B' : go xs
        go (x:xs) = x : go xs
