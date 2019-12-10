module Lib
    ( run
    ) where


run :: IO ()
run = do
  input <- lines <$> readFile "src/input.txt"
-- print $ (astrCoor input (1,0))
  print $ findMax input

findMax amap = maximum $ map (\point -> count amap point) $ astrCoor2 amap

count amap point = length $ removeShadowed (astrCoor amap point) []

removeShadowed [x] acc = x:acc
removeShadowed (x:xs) acc =
  case shadowed of
    True  -> removeShadowed xs acc
    False -> removeShadowed xs (x:acc)
    where shadowed = or $ [inOneLine x x' | x' <- acc]
          res = [inOneLine x x' | x' <- acc]

inOneLine::  (Int, Int) -> (Int, Int) -> Bool
inOneLine (x, 0) (x',0)   = x*x' > 0
inOneLine (_, 0) (_,_)    = False
inOneLine (_, _) (_,0)    = False
inOneLine (0, y) (0, y')  = y*y' > 0
inOneLine (x, y) (x', y') = sameTangent && sameQuarter
                            where sameTangent = ((fromIntegral x)/ (fromIntegral y) == (fromIntegral x')/ (fromIntegral y'))
                                  sameQuarter = (x*x'>=0) && (y*y'>0)


astrCoor:: [[Char]] -> (Int, Int) -> [(Int, Int)]
astrCoor amap (cx, cy) =
  let astrCoorInRowXY = astrCoorInRow (cx,cy)
  in filter ((/=) (0,0)) $ concat $ map astrCoorInRowXY $ zip amap [0..]

astrCoorInRow :: (Int, Int) ->( [Char], Int) -> [(Int, Int)]
astrCoorInRow (cx, cy) (line, y) =
  [(x - cx, cy - y) | (x, obj) <- zip [0..] line, obj == '#' ]


astrCoor2:: [[Char]] -> [(Int, Int)]
astrCoor2 amap =
  let astrCoorInRowXY = astrCoorInRow2
  in filter ((/=) (0,0)) $ concat $ map astrCoorInRowXY $ zip amap [0..]

astrCoorInRow2 :: ( [Char], Int) -> [(Int, Int)]
astrCoorInRow2 (line, y) =
  [(x, y) | (x, obj) <- zip [0..] line, obj == '#' ]


  -- removeShadowed [x] acc = x:acc
  -- removeShadowed (x:xs) acc =
  --   case shadowed of
  --     True  -> removeShadowed xs acc
  --     False -> removeShadowed xs ((x,res):acc)
  --     where shadowed = or $ map (\(x,_) -> x) res
  --           res = [inOneLine x x' | x' <- accc]
  --           accc = map (\(x, _) -> x)  acc
  --
  -- -- inOneLine::  (Int, Int) -> (Int, Int) -> (Bool, Int, Int, Int)
  -- inOneLine (x, 0) (x',0)   = (x*x' > 0, [1,x,0,x',0])
  -- inOneLine (_, 0) (_,_)    = (False, [1,99,0,99,99])
  -- inOneLine (_, _) (_,0)    = (False, [1,99,99,99,0])
  -- inOneLine (0, y) (0, y')  = (y*y' > 0, [1,0,y,0,y'])
  -- inOneLine (x, y) (x', y') = ((div x y == div x' y') && (x*x'>0) && (y*y'>0), [1,x,y,x',y'])
