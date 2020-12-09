import qualified Data.IntSet as S
import Data.Array

sample = "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"

data OPCODE = NOP | ACC | JMP deriving Show
type INSTR = (OPCODE,Int)

-- map readln $ lines src

readln :: String -> INSTR
readln st = (dec w1, sign s (read w2))
  where
    [w1,s:w2] = words st
    dec "nop" = NOP
    dec "acc" = ACC
    dec "jmp" = JMP
    sign '+' = id
    sign '-' = negate

type State = (Int,Int) -- PC, ACC

step :: Array Int INSTR -> State -> State
step prog (pc,acc) = case prog ! pc of
    (NOP,_) -> (succ pc, acc)
    (ACC,i) -> (succ pc, acc+i)
    (JMP,i) -> (pc+i, acc)

run src = loop (0,0) S.empty
  where
    code = map readln $ lines src
    pcmax = pred $ length code
    prog = listArray (0,pcmax) code
    loop st@(pc,acc) pcset
      | S.member pc pcset = acc
      | True = loop (step prog st) (S.insert pc pcset)

test1 = run sample

ans1 = do
  co <- readFile "input.txt"
  print $ run co

run2 prog = loop (0,0) S.empty
  where
    pcmax = snd $ bounds prog
    loop st@(pc,acc) pcset
      | S.member pc pcset = Left acc
      | pc == pcmax = Right $ snd $ step prog st
      | True = loop (step prog st) (S.insert pc pcset)

mod_and_run src =
    [ res
    | (i,inst@(op,arg)) <- zip [0..pcmax] code
    , op' <- par op
    , let prog = prog0 // [(i,(op',arg))]
    , Right res <- [run2 prog]
    ]
  where
    code = map readln $ lines src
    pcmax = pred $ length code
    prog0 = listArray (0,pcmax) code

-- pro action replay
par NOP = [JMP]
par JMP = [NOP]
par _ = []

test2 = mod_and_run sample

ans2 = do
  co <- readFile "input.txt"
  print $ mod_and_run co

{-
*Main> test1
5
*Main> ans1
1867
*Main> test2
[8]
*Main> ans2
[1303]
-}
