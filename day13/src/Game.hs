{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Game
    ( run
    ) where

import           Data.List    as L
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Intcode      as I

run :: IO ()
run = do
  program :: [Int] <- fmap read . fmap T.unpack . T.splitOn "," <$> TIO.readFile "src/input.txt"
  let prg = program ++ (replicate 1000 0)
  print $ play 0 $ I.run (prg, 0,0,[])

-- play2 c yellow
--     -- | c == 1 = output
--     -- | x == (-1) && y == 0 = play2 (c+1) $ I.processInput newPrg $ moveJoy screen
--     | x == (-1) && y == 0 = (screen, joyCoor screen, ballCoor screen, moveJoy screen)
--     | otherwise           = play2 c $ I.processInput newPrg $ moveJoy screen
--   where output = fst yellow
--         newPrg = snd yellow
--         screen = mygroup [] output
--         x = output!!2
--         y = output!!1
--         score = output!!0

-- play :: I.Program -> I.Input -> [Int]
-- 1 = [0,0,-1]],[3,24,20],[4,21,18],1)


play c yellow
    | c == 1 = (screen, joyCoor screen, ballCoor screen, moveJoy screen)
    -- | x == (-1) && y == 0 = play (c+1) $ I.processInput newPrg $ moveJoy screen
    -- | x == (-1) && y == 0 = (output, joyCoor screen, ballCoor screen, moveJoy screen)
    | otherwise           = play (c+1) $ I.processInput newPrg $ moveJoy screen
  where output = fst yellow
        newPrg = snd yellow
        screen = mygroup [] output
        x = output!!2
        y = output!!1
        score = output!!0


moveJoy screen
  | last (joyCoor screen) == last (ballCoor screen) = 0
  | last (joyCoor screen) < last (ballCoor screen) = -1
  | last (joyCoor screen) > last (ballCoor screen) = 1

joyCoor screen= findCoor 3 screen
ballCoor screen= findCoor 4 screen

findCoor n screen =
  case mcoor of
    Just coor -> coor
    Nothing   -> [98798790879807]
  where mcoor = find (\tile -> head tile == n) $ reverse screen

mygroup :: [[Int]] -> [Int] -> [[Int]]
mygroup acc ls
  | length ls == 3 = ls:acc
  | length ls > 3 = mygroup ((take 3 ls):acc) (drop 3 ls)

balled tiles =
  filter yellow tiles
  where yellow tile = notElem [tile!!0,tile!!1,4] tiles
