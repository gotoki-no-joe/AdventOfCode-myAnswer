{-
だいぶ時間も経ったので、改めて作るか。
いやらしい、day23のtgl命令はないようなので、day12の基本命令とoutだけ対応すればよい。
outの結果を見るだけでなく、4つのレジスタの内容も同時に記録して、
それがループしたら成功、という感じで実験するか、
メタ実行してaの値を決める、は辛そうだからね。できたらかっこいいけど。
-}

import Data.Array
import qualified Data.Set as S

runner i f = do
  ls <- lines <$> readFile i
  let ans = f ls
  print ans

main1 = runner "input.txt" part1

part1 ls = head [ini | ini <- [1 ..], check 0 S.empty $ prog $ listArray ('a','d') [ini,0,0,0]]
  where
    prog = assemble ls
    check _ _ [] = False
    check p m ((q, regs):rest)
      | p /= q = False
      | S.member regs m = True
      | otherwise = check (1 - p) (S.insert regs m) rest

-- data Regs = Regs {a :: Int, b :: Int, c :: Int, d :: Int}
type Regs = Array Char Int -- ('a','d')

assemble ls = prog ! 0
  where
    len = length ls
    prog = listArray (0, len) $ zipWith f [0 ..] $ ls ++ ["stop"]
    isReg x = elem x "abcd"
    stop = const []
    f pc l = case words l of
      ["stop"]                    -> stop
      ["cpy", x:_, y:_] | isReg x -> \regs -> next (regs // [(y, regs ! x)])
      ["cpy", lit, y:_]           -> \regs -> next (regs // [(y, read lit)])
      ["inc", x:_]                -> \regs -> next (regs // [(x, succ $ regs ! x)])
      ["dec", x:_]                -> \regs -> next (regs // [(x, pred $ regs ! x)])
      ["jnz", x:_, lit] | isReg x ->
        let dest = pc + read lit
            jump = if inRange (0, len) dest then prog ! dest else stop
        in  \regs -> (if regs ! x == 0 then next else jump) regs
      ["jnz", "0", lit]           -> next
      ["jnz", _  , lit]           ->
        let dest = pc + read lit
            jump = if inRange (0, len) dest then prog ! dest else stop
        in  jump
-- yがレジスタの場合はないのでサボる
      ["out", x:_] | isReg x      -> \regs -> (regs ! x, elems regs) : next regs
      _ -> error $ show (pc, l)
      where
        next = if pc == len then stop else prog ! succ pc
{-
適当にインタプリタで動かそうと思ったけど、
Haskellコードに直して、CPSで次の命令に飛ばすのはできるかな？
cpy lit r = rest (reg {r = lit})
cpy s r   = rest (reg {r = s reg})
inc/dec r = rest (reg {r = succ/pred r reg})
jnz lit y = ...

分岐先が定数なら、いずれかの分岐命令が指しているところだけを関数に取り出して…
いや、なんにせよ配列で指しておけば勝手に解決されるか。ふふ。

jnz lit0 y = freeze!
jnz lit-non0 _y = go y reg
jnz x y = (if x reg == 0 then rest else go y) reg

out x = (output x, a,b,c,d) rest reg
外側でこれを監視し、0,1,0,1でなければ打ち切り。
正しい列が続くとき、このパターンをメモっておき、
再出現したとき成功。これで止まれるかな？

無限ループでなく実行停止する場合もあるので、それに対応しないとランタイムエラーになってしまう。

out命令は一つしかなかったから油断していた。
もし複数あったら、どの位置のものかも気にする必要があった。
というかそのとき、繰り返しの検出はさらにつらいな。
immutableなシステムなので、一度 out r したら、状態が同じなら常に同じ結果になるから、
「次に違う値を出す」心配はしなくていい。つまりレジスタの状態だけ記録しておけばいい。

そして割とあっさり見つかった。
-}
