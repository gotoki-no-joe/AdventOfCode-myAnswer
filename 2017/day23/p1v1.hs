{-
set X Y sets register X to the value of Y.
sub X Y decreases register X by the value of Y.
mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
jnz X Y jumps with an offset of the value of Y, but only if the value of X is not zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
Only the instructions listed above are used. The eight registers here, named a through h, all start at 0.

-}

import Data.Array

parse = map words . lines

type Prog = Array Int [String]
type Regs = Array Char Int
type State = (Int, Regs,Int) -- PC, Regs, count of mul instruction

step :: Prog -> State -> Maybe State
step prog (pc,regs,cnt)
  | pc < 0 || (snd $ bounds prog) < pc = Nothing
  | True = case prog ! pc of
      ("set":[x]:ys:_) -> Just (succ pc, regs // [(x, getVal ys regs)], cnt)
      ("sub":[x]:ys:_) -> Just (succ pc, regs // [(x, regs ! x - getVal ys regs)], cnt)
      ("mul":[x]:ys:_) -> Just (succ pc, regs // [(x, regs ! x * getVal ys regs)], succ cnt)
      ("jnz":xs:ys:_)  -> Just (if getVal xs regs /= 0 then pc + getVal ys regs else succ pc, regs, cnt)

getVal [c] regs
  | 'a' <= c, c <= 'h' = regs ! c
getVal xs _ = read xs

run xs = runloop (0,regs0,0)
  where
    xss = parse xs
    prog = listArray (0, pred $ length xss) xss
    regs0 = accumArray (+) 0 ('a','h') []
    runloop st = case step prog st of
        Nothing -> st
        Just st1 -> runloop st1

main1 = do
  co <- readFile "input.txt"
  return $ run co

{-
*Main> main1
(32,array ('a','h') [('a',0),('b',57),('c',57),('d',57),('e',57),('f',0),('g',0),('h',1)],3025)
-}
