{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Lib
    ( run
    ) where

-- import qualified Data.HashMap    as H
import           Data.List       as L
import           Data.List.Split as S
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO

run :: IO ()
run = do
  input <- fmap T.unpack . T.splitOn "\n" <$> TIO.readFile "src/tinput.txt"
  let hash = toH input
  print $ calc hash $ findReaction hash "FUEL"


toH :: [String] -> [(String, (Int, [String]))]
toH input = format <$> S.splitOn " => " <$> L.init input


calc hash (result, (nOfResult, ings)) = (nOfResult,yy)
  where yy = react hash <$> ings


--
-- react hash ing =
--   case result of
--           -- "ORE"   -> 1
--           result' -> yellow nOfIng hash result'
--           -- result' -> n * (calc hash $ findReaction hash result')
--   where result = ing'!!1
--         nOfIng :: Int = read $ ing'!!0
--         ing' = S.splitOn " " ing
--
-- yellow nOfResult hash result = (nn,yy)
--   where (nOfIng, yy) = calc2 hash (findReaction hash result)
--         nn = ceiling $ (fromIntegral nOfResult) / (fromIntegral nOfIng)
--         rest = nn - (div nOfResult nOfIng)
--
-- calc2 hash (result, (nOfResult, ings)) = (nOfResult,yy)
--   where yy = react2 hash <$> ings
--
-- react2 hash ing =
--   case result of
--           -- "ORE"   -> 1
--           result' -> yellow2 nOfIng hash result'
--           -- result' -> n * (calc hash $ findReaction hash result')
--   where result = ing'!!1
--         nOfIng :: Int = read $ ing'!!0
--         ing' = S.splitOn " " ing
--
-- -- yellow2 3 hash "A"
-- yellow2 nOfResult hash result = (nn, ings)
--   where (nOfIng, ings) = nreaction
--         nreaction = (findReaction hash result)
--         nn = ceiling $ (fromIntegral nOfResult) / (fromIntegral nOfIng)
--         rest = nn - (div nOfResult nOfIng)





format reac = (x!!1, (n, y))
  where n = read $ x!!0
        x = S.splitOn " " $ reac!!1
        y = S.splitOn ", " $ reac!!0

findReaction hash result =
  case find (\(result', _) -> result'==result ) hash of
    Just reaction -> reaction
    Nothing       -> ("", (0, []))
