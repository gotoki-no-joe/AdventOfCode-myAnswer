# 入力

原子記号は大文字1文字または大文字1文字+小文字1文字でひとつ。
原子記号一つをひとつの整数で扱うよう変換する。
16ビットもあれば表現できるので、`Word16` でも収まる。
（実際に現れている記号だけに背番号をつければ、`Word8`でも収まるだろうけど。）

```haskell
import Data.Char

encode :: String -> [Int]
encode (c1:c2:cs) | isLower c2 = ord c1 * 256 + ord c2 : encode cs
encode (c:cs) = ord c : encode cs
encode "" = []
```

最終行以外は置換を表す。
一つの置換は、原子 `Int` から原子の列 `[Int]` へ置換する。

最終行は原子の列 `[Int]` で表せる。

```
parse :: String -> (Int,[Int])
parse xs = (head $ encode w1, encode w3)
  where
    [w1,_,w3] = words xs

runner i f = do
  ls <- lines <$> readFile i
  let n = length ls
      rules = map parse $ take (n - 2) ls
      mol = encode $ last ls
  print $ f rules mol
```

# パート1

置換の左辺は重複があるので、それら全ての可能性を試し、
初期列のいずれかの原子に対して可能な置き換えを一度行った全ての結果を作り、
重複を除いて種類を数える。

```haskell
import qualified Data.IntMap as IM
import qualified Data.Set as S
import Data.List

part1 rules mol = S.size mols
  where
    rs = IM.fromListWith (++) [(l,[r]) | (l,r) <- rules]
    mols = S.fromList [a ++ d ++ c | (a,b:c) <- zip (inits mol) (tails mol), d <- IM.findWithDefault [] b rs]

main1 = runner "input.txt" part1
```

# パート2

本当に原子から出発して、あらゆる置換を幅優先探索で試すのは発散する。
目標に到達するように置換の適用を制御する方法もよくわからない。

完成した瞬間から時計を戻して考えて、目標から逆変換して原子まで還元するステップを探す。

もし変換規則が優しく作られていて、端から貪欲に書き換えを行えばよいならば、話は楽。
さもなくば、可能な還元を実行して作られる分子の全てを幅優先で作っていく全探索が必要になる。

規則が膨大なら、右辺のTrieを構築するところだが、そこまでではないので、
右辺の先頭の原子をキーにして整理しておくにとどめる。

```haskell
part2 rules mol0 = ...
  where
    revrules = IM.fromListWith (++) [(r1, [lr]) | lr@(l,r1:_) <- rules]
```

これを使って、分子に可能な還元を行った全ての結果を作る計算が定義できる。

```haskell
    revstep mol =
      [ a ++ l : drop (length r) c
      | (a,b:c) <- zip (inits mol) (tails mol)
      , (l,_:r) <- IM.findWithDefault [] b revrules
      , isPrefixOf r c]
```

可能な限り `revstep` を適用し、その先頭だけ残す、を繰り返し、毎回の分子長さ、繰り返しの回数、最終結果の分子、を見てみる。

```haskell
import Debug.Trace

part2 rules mol0 = loop 0 mol0
  where
    loop cnt mol = traceShow (cnt, length mol) $
      case revstep mol of
        [] -> (cnt, mol)
        (mol1:_) -> loop (succ cnt) mol1
```

残念ながら201ステップで長さ15の分子で進まなくなってしまった。
丁寧に幅優先探索する必要があるようだ。

既出の分子は探索しないように記録しつつ、深さ優先探索で電子を目指す。

```haskell
part2 rules mol0 = iter S.empty [(0, mol0)]
  where
    revrules = IM.fromListWith (++) [(r1, [lr]) | lr@(l,r1:_) <- rules]
    revstep mol =
      [ a ++ l : drop (length r) c
      | (a,b:c) <- zip (inits mol) (tails mol)
      , (l,_:r) <- IM.findWithDefault [] b revrules
      , isPrefixOf r c]

    electron = encode "e"

    iter :: S.Set [Int] -> [(Int,[Int])] -> Int
    iter dones ((cnt,mol):cmols)
      | electron == mol    = cnt
      | S.member mol dones = iter dones cmols
      | otherwise          = iter dones1 cmols1
      where
        mol1s = revstep mol
        cnt1 = succ cnt
        dones1 = S.insert mol dones
        cmols1 = foldr step cmols mol1s
        step m r
          | S.member m dones1 = r
          | otherwise = (cnt1, m) : r
```

何とか答えに到達した。
実際、可能な還元リストの末尾を辿るようにすると一直線に到達するので、
相当いやらしい設計になっているようだ。すごい。

そして、これはDFSで一つの解を見つけただけなので、これが最短であるという保証はない。
証明するためには、全ての場合を続けて探索し尽くす必要があるが、これが終わらない。
なのでこのspoilerは不完全な内容。
