module Lib
    ( result
    ) where

result :: Int -> [Int] -> [Int]
result input program = run 0 input [] initAddress program

initAddress = 0

run :: Int -> Int -> [Int] -> Int -> [Int] -> [Int]
run base input output idx prg
    | opc == 99   = output
    | opc == 1    = run base input output (idx+4) (execute base idx prg (+))
    | opc == 2    = run base input output (idx+4) (execute base idx prg (*))
    | opc == 3    = run base input output (idx+2) (replaceNth (prg!!(idx+1)+base) input prg)
    | opc == 4    = run base input (myread base (idx+1) prg (mode1 idx prg) : output) (idx+2) prg
    | opc == 5    = run base input output (jumpTo base idx prg (/=)) prg
    | opc == 6    = run base input output (jumpTo base idx prg (==)) prg
    | opc == 7    = run base input output (idx+4) (compareAndSave base idx prg (<))
    | opc == 8    = run base input output (idx+4) (compareAndSave base idx prg (==))
    | opc == 9    = run (newBase base idx prg) input output (idx+2) prg
    | otherwise   = [-1]
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

myread :: Int -> Int -> [Int] -> Int -> Int
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
