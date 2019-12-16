{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Robot
    ( run
    ) where

import           Control.Concurrent
import           Data.List          as L
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import qualified Intcode            as I


type Coor = (Int, Int)
initCoor :: Coor = (0,0)
type SectionMap = [[Char]]
initMap :: SectionMap = ["B"]
type Dir = Int
initDir :: Dir = 1

run :: IO ()
run = do
  input :: [Int] <- fmap read . fmap T.unpack . T.splitOn "," <$> TIO.readFile "src/input.txt"
  let prg = (input ++ (replicate 1000 0), 0,0,0)
  let (rCoor, prg', dir, sectionMap) = startSearch prg
  display rCoor prg' dir sectionMap

display :: Coor -> I.Program -> Dir -> SectionMap -> IO ()
display rCoor prg dir sectionMap = do
  let (rCoor', prg', dir', sectionMap', obj') = search rCoor prg dir sectionMap
  mapM putStrLn $ drawRobot rCoor' $ sectionMap'
  print (rCoor', dir', obj')
  -- threadDelay 100000
  case obj' of
      2                  -> display rCoor' prg' dir' sectionMap'
      n | n `elem` [0,1] -> display rCoor' prg' dir' sectionMap'
      n                  -> error $ "object out of range in v: " ++ show n

startSearch :: I.Program -> (Coor, I.Program, Dir, SectionMap)
startSearch prg =
  (adjustedRCoor, prg', dir, sectionMap)
  where   dir = findNextDir initDir obj
          (adjustedRCoor, sectionMap) = mark rCoor objCoor initMap obj
          (rCoor, objCoor, prg', obj) = move (initCoor, prg) initDir

search :: Coor -> I.Program -> Dir -> SectionMap -> (Coor, I.Program, Dir, SectionMap, Int)
search rCoor prg dir sectionMap =
  (adjustedRCoor', prg', dir', sectionMap', obj')
  where
        dir' = findNextDir dir obj'
        (adjustedRCoor', sectionMap') = mark rCoor' objCoor' sectionMap obj'
        (rCoor', objCoor', prg', obj') = move (rCoor, prg) dir

findNextDir :: Int -> Int -> Int
findNextDir dir obj =
  case obj of
    1                  -> case dir of
                            1 -> 3 -- N -> N
                            4 -> 1 -- E -> N
                            2 -> 4 -- S -> E
                            3 -> 2 -- w -> S
    n | n `elem` [0,2] -> case dir of
                            1 -> 4 -- N -> E
                            4 -> 2 -- E -> S
                            2 -> 3 -- S -> W
                            3 -> 1 -- w -> N
    n                  -> error $ "object out of range in findNextDir: " ++ show n

move :: (Coor, I.Program) -> Int -> (Coor, Coor, I.Program, Int)
move (coor, prg) dir =
  (robotCoor, objCoor, newPrg, obj)
  where robotCoor = moveRobot coor obj dir
        objCoor = moveObj coor dir
        (_,_,_,obj) = newPrg
        newPrg = I.processInput prg dir

mark :: Coor -> Coor -> SectionMap -> Int -> (Coor, SectionMap)
mark robotCoor objCoor sectionMap obj =
  case (adjMap!!y!!x) of
    'B' -> (adjRCoor, adjMap)
    c   ->  (adjRCoor, replaceNth y newLine adjMap)
  where newLine = replaceNth x obj' (adjMap!!y)
        obj' = objSign obj
        (x, y) = adjOCoor
        adjRCoor = adjustCoor objCoor robotCoor
        adjOCoor = adjustCoor objCoor objCoor
        adjMap = adjustMap sectionMap objCoor

adjustCoor :: Coor -> Coor -> Coor
adjustCoor (x,y) (ox, oy) = (nx,ny)
  where nx = adjustCoorX x ox
        ny = adjustCoorX y oy

adjustCoorX :: Int -> Int -> Int
adjustCoorX x ox = nx
  where nx | x<0 = ox - x
           | otherwise = ox

adjustMap :: SectionMap -> Coor -> SectionMap
adjustMap sectionMap (x,y) =
  adjustMapX x $ adjustMapY y sectionMap

adjustMapX :: Int -> SectionMap -> SectionMap
adjustMapX x sectionMap
  | x < 0 = (\l -> newPointsWest ++ l) <$> sectionMap
  | x >= maxX = (\l -> l ++ newPointsEast) <$> sectionMap
  | otherwise = sectionMap
  where
        maxX = maximum $ length <$> sectionMap
        newPointsEast = newPoints (x - maxX +1)
        newPointsWest = newPoints (abs x)
        newPoints n = replicate n ' '

adjustMapY :: Int -> SectionMap -> SectionMap
adjustMapY y sectionMap
  | y < 0 = newLinesNorth ++ sectionMap
  | y >= maxY = sectionMap ++ newLinesSouth
  | otherwise = sectionMap
  where
        maxY = length sectionMap
        maxX = maximum $ length <$> sectionMap
        newLinesNorth = replicate (abs y) newLine
        newLinesSouth = replicate (y - maxY + 1) newLine
        newLines n = replicate n newLine
        newLine = replicate maxX ' '

objSign obj =
  case obj of
    0 -> '*'
    1 -> '.'
    2 -> '$'
    n -> error $ "object out of range in objSign: " ++ show n

moveObj (x,y) dir =
  case dir of
    1 -> (x,y-1)
    2 -> (x,y+1)
    3 -> (x-1,y)
    4 -> (x+1,y)
    n -> error $ "dir out of range in moveObj: " ++ show n

moveRobot (x,y) obj dir =
  case obj of
    0                  -> (x,y)
    n | n `elem` [1,2] -> moveObj (x,y) dir
    n                  -> error $ "object out of range in moveRobot: " ++ show n


drawRobot (x,y) sectionMap =
  replaceNth y newLine sectionMap
  where newLine = replaceNth x '@' (sectionMap!!y)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs
