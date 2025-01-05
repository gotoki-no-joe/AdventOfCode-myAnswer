# 入力

1行目はそのまま文字列として。

それ以降は、現在位置を表す3文字の文字列をキーとして、ペアをそのままペアとする`Map`に。

```haskell
import qualified Data.Map as M

runner i f = do
  l1:_:ls <- lines <$> readFile i
  let m = M.fromList $ map parse ls
  print $ f l1 m

parse :: String -> (String,(String,String))
parse l = (take 3 l, (take 3 $ drop 7 l, take 3 $ drop 12 l))

test1 = runner "samp1.txt" part1
main1 = runner "input.txt" part1

part1 l1 m = ...
```

# パート1

無限に繰り返す指示に従って、`AAA`から始めて`ZZZ`に至るまでのステップ数を数える。
書いてあるとおり。

```haskell
part1 lrs m = loop 0 "AAA" (cycle lrs)
  where
    loop cnt "ZZZ" _ = cnt
    loop cnt pos (x:xs) = loop (succ cnt) pos1 xs
      where
        pos1 = (if x == 'L' then fst else snd) $ m M.! pos
```

# パート2

全員を本当に同時に走らせると、多分大変なステップ数になるのでやめよう。

最小公倍数的な計算が必要になるのだろうけど、本当にそれでよいか、
それぞれの `**A` が `**Z` や `**A` をどう辿るか観察してみる。

`**A`や`**Z`に到着したときは位置とカウントを出力、
到着した `**A`や`**Z`を記録し、2度めの訪問で終了。

あと、コマンド列の長さも見ておく。

```haskell
import qualified Data.Set as S

check1 lrs m = unlines $ (show $ length lrs) : map (\p -> loop S.empty 0 p (cycle lrs)) starts
  where
    starts = filter (('A' ==) . last) $ M.keys m
    loop vis cnt pos (x:xs) = disp ++ cont
      where
        isAZ = elem (last pos) "AZ"
        pos1 = (if x == 'L' then fst else snd) $ m M.! pos
        vis1 = if isAZ then S.insert pos vis else vis
        cont = if S.member pos vis then "" else loop vis1 (succ cnt) pos1 xs
        disp = if isAZ then unwords [show cnt, pos, "- "] else ""
```

```
283
0 AAA - 12169 ZZZ - 24338 ZZZ -
0 BVA - 17263 MLZ - 34526 MLZ -
0 HHA - 20093 TNZ - 40186 TNZ -
0 HVA - 14999 RFZ - 29998 RFZ -
0 NPA - 20659 BXZ - 41318 BXZ -
0 RSA - 16697 KPZ - 33394 KPZ -
```

ふむ。

```haskell
ghci> map (2 *) [12169, 17263, 20093, 14999, 20659, 16697]
[24338,34526,40186,29998,41318,33394]
ghci> map (flip divMod 283) [12169, 17263, 20093, 14999, 20659, 16697]
[(43,0),(61,0),(71,0),(53,0),(73,0),(59,0)]
ghci> foldr lcm 1 [12169, 17263, 20093, 14999, 20659, 16697]
12030780859469
```

<!--
過去の自分は、コマンド列の長さを1読み間違えてドツボにはまっていたようだ。
-->
