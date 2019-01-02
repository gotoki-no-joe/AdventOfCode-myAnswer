import Data.Array

main = do
  fi <- readFile "input.txt"
  let src = map parse $ lines fi
  let prog = listArray (0,length src - 1) src
  let (ip,a,b) = run prog [0,0]
  print b
  let (ip,a,b) = run prog [1,0]
  print b

type Reg = Int
data Inst = Ihlf Reg | Itpl Reg | Iinc Reg | Ijmp Int | Ijie Reg Int | Ijio Reg Int
  deriving Show

parse :: String -> Inst

parse cs =
  let
    ca = words cs
    r = if head (ca !! 1) == 'a' then 0 else 1
    ca1 = ca !! 1
    o1 = read $ if head ca1 == '+' then tail ca1 else ca1
    ca2 = ca !! 2
    o2 = read $ if head ca2 == '+' then tail ca2 else ca2
  in
    case ca !! 0 of
      "hlf" -> Ihlf r
      "tpl" -> Itpl r
      "inc" -> Iinc r
      "jmp" -> Ijmp o1
      "jie" -> Ijie r o2
      "jio" -> Ijio r o2

run prog rs0 = exec 0 (listArray (0,length rs0 - 1) rs0) where
  (lb,ub) = bounds prog
  exec :: Int -> Array Int Int -> (Int,Int,Int)
  exec ip regs
    | ip < lb || ub < ip = (ip,regs ! 0,regs ! 1)
    | otherwise = case prog ! ip of
        Ihlf r -> let r1 = regs // [(r, regs ! r `div` 2)] in exec (succ ip) r1
        Itpl r -> let r1 = regs // [(r, regs ! r * 3)]     in exec (succ ip) r1
        Iinc r -> let r1 = regs // [(r, succ (regs ! r))]  in exec (succ ip) r1
        Ijmp ofs -> exec (ip + ofs) regs
        Ijie r ofs -> exec ((if even (regs ! r) then (ofs +) else succ) ip) regs
        Ijio r ofs -> exec ((if 1 == (regs ! r) then (ofs +) else succ) ip) regs

{-
*Main> main
184
231

特になんのもりあがりもなく終わった。
-}
