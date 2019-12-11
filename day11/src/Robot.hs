{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Robot
    ( run
    ) where

import           Data.Set     as S
import qualified Data.Text    as T
import           Data.Text.IO as TIO
import qualified Intcode      as I

initIdx = 0
initBase = 0
initVisitedFields = S.empty
initWhitePanels = S.empty
initCoor = (0,0)
initDir = 0
type Coor = (Int, Int)
type Dir = Int
type Robot = (Coor, Dir)

run = do
    program :: [Int] <- fmap read . fmap T.unpack . T.splitOn "," <$> TIO.readFile "src/input.txt"
    print $ work (program, initIdx, initBase) initVisitedFields initWhitePanels (initCoor, initDir)

work :: I.Program -> S.Set Coor-> S.Set Coor-> Robot -> Int
work prg vf wp (currentCoor, currentDir) =
  let newVF = S.insert currentCoor vf
      currentColor = panelColor wp currentCoor

      (color, prg2) = I.run prg currentColor
      newWF = paintPanel wp currentCoor color
      finished = color == 99

  in case finished of
    True -> length vf
    False -> let (rotation, prg3) = I.run prg2 0
                 newDir = mod (currentDir - 1 + (rotation * 2)) 4
                 newCoor = move currentCoor newDir
              in work prg3 newVF newWF (newCoor, newDir)


move :: Coor -> Dir -> Coor
move (x,y) newDir =
  case newDir of
     0 -> (x, y+1)
     1 -> (x+1, y)
     2 -> (x, y-1)
     3 -> (x-1, y)

panelColor :: Set Coor -> Coor -> Int
panelColor wp square =
  case isWhite of
    True  -> 1
    False -> 0
  where isWhite = S.member square wp

paintPanel :: Set Coor -> Coor -> Int -> Set Coor
paintPanel wp square color =
  case color of
    0 -> S.delete square wp
    1 -> S.insert square wp
