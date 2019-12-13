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
  let prg = program ++ (replicate 10000 0)
  print $ length $ filter (\tile -> head tile == 2) $ L.nub $ mygroup [] $ snd $ I.runD prg

mygroup :: [[Int]] -> [Int] -> [[Int]]
mygroup acc ls
  | length ls == 3 = ls:acc
  | length ls > 3 = mygroup ((take 3 ls):acc) (drop 3 ls)

balled tiles =
  filter yellow tiles
  where yellow tile = notElem [tile!!0,tile!!1,4] tiles
