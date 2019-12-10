module Lib
    ( run1,
    run2
    ) where


import           Data.Sort

-- second star --

run2 :: IO ()
run2 = do
  astroidMap <- lines <$> readFile "src/input.txt"
  let (_, bestPoint) = findMax astroidMap
  let allVisible = sortOn comparison $ removeShadowed (asteroidCoordsRelative astroidMap bestPoint) []
  let (x,y) = allVisible!!199
  let (x', y') = bestPoint
  print $ (x+x',y'-y)

comparison (x,y)
  | (x==0 && y>0) = (1,0)
  | (x>0 && y>0) = (1, (fromIntegral x)/ (fromIntegral y))
  | (x>0 && y==0) = (1, 9999999999999)
  | (x>0 && y<0) = (2, (fromIntegral x)/ (fromIntegral y))
  | (x==0 && y<0) = (2, 0)
  | (x<0 && y<0) = (3, (fromIntegral x)/ (fromIntegral y))
  | (x<0 && y==0) = (3, 9999999999999)
  | (x<0 && y>0) = (4, (fromIntegral x)/ (fromIntegral y))

-- first star --

run1 :: IO ()
run1 = do
  astroidMap <- lines <$> readFile "src/input.txt"
  print $ findMax astroidMap

findMax astroidMap = maximum $ map (\point -> ((count astroidMap point),point)) $ asteroidCoords astroidMap

count astroidMap point = length $ removeShadowed (asteroidCoordsRelative astroidMap point) []

removeShadowed [x] acc = x:acc
removeShadowed (x:xs) acc =
  case shadowed of
    True  -> removeShadowed xs acc
    False -> removeShadowed xs (x:acc)
    where shadowed = or $ [inOneLine x x' | x' <- acc]

inOneLine::  (Int, Int) -> (Int, Int) -> Bool
inOneLine (x, 0) (x',0)   = x*x' >= 0
inOneLine (_, 0) (_,_)    = False
inOneLine (_, _) (_,0)    = False
inOneLine (x, y) (x', y') = sameTangent && sameQuarter
                            where sameTangent = ((fromIntegral x)/ (fromIntegral y) == (fromIntegral x')/ (fromIntegral y'))
                                  sameQuarter = (x*x'>=0) && (y*y'>0)


asteroidCoordsRelative:: [[Char]] -> (Int, Int) -> [(Int, Int)]
asteroidCoordsRelative astroidMap (cx, cy) =
  let asteroidCoordsRelativeInRowXY = asteroidCoordsRelativeInRow (cx,cy)
  in filter ((/=) (0,0)) $ concat $ map asteroidCoordsRelativeInRowXY $ zip astroidMap [0..]

asteroidCoordsRelativeInRow :: (Int, Int) ->( [Char], Int) -> [(Int, Int)]
asteroidCoordsRelativeInRow (cx, cy) (line, y) =
  [(x - cx, cy - y) | (x, obj) <- zip [0..] line, obj == '#' ]


asteroidCoords:: [[Char]] -> [(Int, Int)]
asteroidCoords astroidMap =
  let asteroidCoordsInRowXY = asteroidCoordsInRow
  in filter ((/=) (0,0)) $ concat $ map asteroidCoordsInRowXY $ zip astroidMap [0..]

asteroidCoordsInRow :: ( [Char], Int) -> [(Int, Int)]
asteroidCoordsInRow (line, y) =
  [(x, y) | (x, obj) <- zip [0..] line, obj == '#' ]
