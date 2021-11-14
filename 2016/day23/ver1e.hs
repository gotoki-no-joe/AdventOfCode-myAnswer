{-
Don't bunnies usually multiply?
を「倍にする」と解釈してやってみる。
-}

import Data.Array

data OpCode = Cpy | Jnz | Inc | Dec | Tgl | Mul deriving Show

type Instr = (OpCode,String,String)

main1 = do
  co <- readFile "input.txt"
  let prog = map (parseLine . words) $ lines co
  print $ run prog 7

parseLine :: [String] -> Instr
parseLine (w1:w2:ws) =
  case w1 of
    "cpy" -> (Cpy, w2, head ws)
    "jnz" -> (Jnz, w2, head ws)
    "inc" -> (Inc, w2, "")
    "dec" -> (Dec, w2, "")
    "tgl" -> (Tgl, w2, "")
    "mul" -> (Mul, w2, head ws)

toggle Inc = Dec
toggle Dec = Inc
toggle Tgl = Inc
toggle Jnz = Cpy
toggle Cpy = Jnz

toggleInst (i,o1,o2) = (toggle i, o1, o2)

type State =
  ( Int -- Instruction Pointer
  , Array Int Instr -- Program Memory
  , Array Char Int) -- Register File

step :: State -> Maybe State
step (ip, prog, regs)
  | isIn ip =
    case prog ! ip of
      (Inc, o1, _) -> if isReg o1 then let r = head o1 in Just (succ ip, prog, regs // [(r, succ (regs ! r))]) else next
      (Dec, o1, _) -> if isReg o1 then let r = head o1 in Just (succ ip, prog, regs // [(r, pred (regs ! r))]) else next
      (Tgl, o1, _) -> doTgl $ operand1 o1
      (Cpy, o1, o2) -> if isReg o2 then Just (succ ip, prog, regs // [(head o2, operand1 o1)]) else next
      (Jnz, o1, o2) -> if operand1 o1 /= 0 then Just (ip + operand1 o2, prog, regs) else next
      (Mul, o1, o2) -> Just (succ ip, prog, regs // [('a', regs ! 'a' * regs ! 'b')])
  | otherwise = Nothing -- overrun
  where
    next = Just (succ ip, prog, regs)
    (0,ub) = bounds prog
    isIn x = 0 <= x && x <= ub
    isReg [c] = elem c "abcd"
    isReg _ = False
    operand1 o1 = if isReg o1 then regs ! head o1 else read o1
    doTgl y
      | 2 <= x, x <= 9 = error (show x)
      | isIn x = Just (succ ip, prog // [(x, toggleInst $ prog ! x)], regs)
      | True = next
      where
        x = ip + y
run instrs a0 = regs ! 'a'
  where
    prog0 = listArray (0, pred $ length instrs) instrs
    regs0 = listArray ('a','d') [a0,0,0,0]
    (_,_,regs) = loop (0, prog0, regs0)
    loop st = case step st of
      Just st1 -> loop st1
      Nothing  -> st

test1 = map (parseLine . words) $ lines "cpy 2 a\ntgl a\ntgl a\ntgl a\ncpy 1 a\ndec a\ndec a"
test2 = run test1 7

{-
*Main> test1
[(Cpy,"2","a"),(Tgl,"a",""),(Tgl,"a",""),(Tgl,"a",""),(Cpy,"1","a"),(Dec,"a",""),(Dec,"a","")]
*Main> test2
3
*Main> main1
12000
ひさびさにStarを取った。
-}

main2 = do
  co <- readFile "input2.txt"
  let prog = map (parseLine . words) $ lines co
  print $ run prog 12

{-
*Main> main2
479008560
一瞬で答えは出たけれど、カンニングしてしまった。
-}