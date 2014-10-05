main :: IO ()
main = getContents >>= print . solve . map (map read . words) . lines

solve :: [[Double]] -> Double
solve [[x1, y1], [x2, y2], [x3, y3]] = sides * rsquare * sin (2 * pi / sides) / 2
  where
    (x, y) = center (x1, y1) (x2, y2) (x3, y3)
    (dx1, dy1) = (x1 - x, y1 - y)
    (dx2, dy2) = (x2 - x, y2 - y)
    (dx3, dy3) = (x3 - x, y3 - y)
    a1 = 2 * pi / angle (dx1, dy1) (dx2, dy2)
    a2 = 2 * pi / angle (dx2, dy2) (dx3, dy3)
    a3 = 2 * pi / angle (dx3, dy3) (dx1, dy1)
    sides = head $ filter (\n -> allint (n / a1, n / a2, n / a3)) (map fromIntegral ([3..] :: [Int]))
    allint (a, b, c) = isNearInteger a && isNearInteger b && isNearInteger c
    isNearInteger a = abs (a - fromIntegral (round a :: Integer)) < 1e-4
    rsquare = dx1**2 + dy1**2
solve _ = undefined

center :: Floating t => (t, t) -> (t, t) -> (t, t) -> (t, t)
center (x1, y1) (x2, y2) (x3, y3) = (x, y)
  where x = ((x1**2 + y1**2) * (y2 - y3) + (x2**2 + y2**2) * (y3 - y1) + (x3**2 + y3**2) * (y1 - y2)) / d
        y = ((x1**2 + y1**2) * (x3 - x2) + (x2**2 + y2**2) * (x1 - x3) + (x3**2 + y3**2) * (x2 - x1)) / d
        d = 2 * (x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2))

inner :: Floating a => (a, a) -> (a, a) -> a
inner (dx1, dy1) (dx2, dy2) = dx1 * dx2 + dy1 * dy2

len :: Floating a => (a, a) -> a
len (dx1, dy1) = sqrt (dx1**2 + dy1**2)

angle :: Floating a => (a, a) -> (a, a) -> a
angle d1 d2 = acos (inner d1 d2 / (len d1 * len d2))
