-- import Data.Numbers.Primes
-- import Debug.Trace
{-# Language Strict #-}

{-
part2のみ

家番号とプレゼントの数のタプルを50個生成するエージェントを無限に作り、
足し合わさないものは排出していく形でどうだろう。

自分の番号がわからないと、どこまでは吐き出していいかわからないのでそうする。

そこまでの結果を、次のステップに渡さないといけないのか、なるほど。
-}

elf k = [ (i*k,k) | i <- [2..50] ]

presents = (1,1) : pres 2 (elf 1)

pres k ((i,x):ixs) =
  case compare k i of
--    GT -> trace ("GT "++show k) $ (i,x) : pres k ixs
    EQ -> (i,x+k) : pres (succ k) (merge (elf k) ixs)
    LT -> (k,k) : pres (succ k) (merge (elf k) ixs)

merge ixixs@((i,x):ixs) jyjys@((j,y):jys) =
  case compare i j of
    LT -> (i,x) : merge ixs jyjys
    EQ -> (i,x+y) : merge ixs jys
    GT -> (j,y) : merge ixixs jys
merge ixs [] = ixs
merge [] jys = jys

ans2 = head $ dropWhile ((36000000 >).snd) presents
