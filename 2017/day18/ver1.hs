{-
a-zのレジスタを持っている
「最後に鳴らした音の周波数」もレジスタとして保持するべき
-}

import Data.Array

compile :: String -> Array Int [String]
compile src = listArray (0,pred $ length ls) ls
  where
    ls = map words $ lines src

type State =
  ( Int  -- PC
  , Array Char Int -- レジスタ
  , Maybe Int) -- 最後に鳴らした音の周波数

state0 = (0, accumArray (+) 0 ('a','z') [], Nothing)

step :: State -> [String] -> Either (Maybe Int) State
step (pc,regs,snd) ("snd":xs:_) = Right (succ pc, regs, Just $ getVal xs regs)
step state ("set":xss) = stepReg state xss (flip const)
step state ("add":xss) = stepReg state xss (+)
step state ("mul":xss) = stepReg state xss (*)
step state ("mod":xss) = stepReg state xss mod
step (pc,regs,snd) ("rcv":xs:_) = if getVal xs regs == 0 then Right (succ pc, regs, snd) else Left snd
step (pc,regs,snd) ("jgz":xs:ys:_) = Right (pc + if getVal xs regs > 0 then getVal ys regs else 1, regs, snd)

getVal :: String -> Array Char Int -> Int
getVal [c] regs
  | 'a' <= c, c <= 'z' = regs ! c
getVal xs _ = read xs

stepReg :: State -> [String] -> (Int -> Int -> Int) -> Either (Maybe Int) State
stepReg (pc,regs,snd) ([r]:ys:_) f = Right (succ pc, regs // [(r, f (regs ! r) (getVal ys regs))], snd)

run prog = runloop state0
  where
    (_,ub) = bounds prog
    runloop st@(pc,_,_)
      | pc < 0 || ub < pc = error ("halt" ++ show st)
      | True = case step st (prog ! pc) of
          Left result -> result
          Right st1 -> runloop st1

sample = compile "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2\n"

main1 = do
  co <- readFile "input.txt"
  print $ run $ compile co

{-
*Main> run sample
Just 4
*Main> main1
Just 7071
-}
