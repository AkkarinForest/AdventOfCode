module Intcode
    ( run
    , Program
    ) where

initAddress = 0
initialInput = 2
type Program = ([Int], Idx, Base)
type Output = Int
type Input = Int
type Idx = Int
type Base = Int

run :: Program -> Input -> (Output, Program)
run program input = process input program

process :: Input -> Program -> (Output, Program)
process input (prg, idx, base)
    | opc == 1    = process input ((execute base idx prg (+)), (idx+4), base)
    | opc == 2    = process input ((execute base idx prg (*)), (idx+4), base)
    | opc == 3    = process input ((replaceNth (prg!!(idx+1)+base) input prg), (idx+2), base)
    | opc == 5    = process input (prg, (jumpTo base idx prg (/=)), base)
    | opc == 6    = process input (prg, (jumpTo base idx prg (==)), base)
    | opc == 7    = process input ((compareAndSave base idx prg (<)), (idx+4), base)
    | opc == 8    = process input ((compareAndSave base idx prg (==)), (idx+4), base)
    | opc == 9    = process input (prg, (idx+2), (newBase base idx prg))
    | opc == 4    = ((myread base (idx+1) prg (mode1 idx prg)), (prg, (idx+2), base))
    | opc == 99   = (99, (prg, idx, base))
    | otherwise   = (-1, (prg, idx, base))
    where opc = opcode idx prg

newBase base idx prg =
  let m1 = (mode1 idx prg)
      val1 = (myread base (idx+1) prg m1)
      in base + val1

execute base idx prg fnc =
    let m1 = (mode1 idx prg)
        m2 = (mode2 idx prg)
        m3 = (mode3 idx prg)
        val1 = (myread base (idx+1) prg m1)
        val2 = (myread base (idx+2) prg m2)
        nth = mywrite base (idx+3) prg m3
    in replaceNth nth (fnc val1 val2) prg

jumpTo base idx prg fnc
    | fnc val1 0 = val2
    | otherwise = idx + 3
    where val1 = myread base (idx+1) prg (mode1 idx prg)
          val2 = myread base (idx+2) prg (mode2 idx prg)

compareAndSave base idx prg fnc =
    replaceNth val3 result prg
    where val1 = myread base (idx+1) prg (mode1 idx prg)
          val2 = myread base (idx+2) prg (mode2 idx prg)
          m3 = (mode3 idx prg)
          val3 = mywrite base (idx+3) prg m3
          result
                | fnc val1 val2 = 1
                | otherwise = 0

mywrite base idx prg mode
    | mode == 0 = prg!!(idx)
    -- | mode == 1 = prg!!(idx)
    | mode == 2 = prg!!(idx) + base
    | otherwise = prg!!(idx)

opcode :: Int -> [Int] -> Int
opcode idx prg =
    let code = prg!!idx
    in code - ((div code 100) * 100)

mode1 idx prg =
    let code = prg!!idx
    in div code 100 - (div code 1000) * 10

mode2 idx prg =
    let code = prg!!idx
    in div code 1000 - (div code 10000) * 10

mode3 idx prg =
    let code = prg!!idx
    in div code 10000

myread :: Base -> Idx -> [Int] -> Int -> Int
myread base idx prg mode
    | mode == 0 = prg!!(prg!!(idx))
    | mode == 1 = prg!!(idx)
    | mode == 2 = prg!!(prg!!(idx) + base)
    | otherwise = 0

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

addMemory prg = prg ++ (replicate 10000 0)
