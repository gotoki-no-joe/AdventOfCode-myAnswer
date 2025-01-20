# パート1

pureMD5を利用する。

全体の流れとしては、まずソルトに対して数表現を追加した文字列のハッシュを計算する。
この中から条件を満たすものを抜き出すのだが、
MD5ハッシュの生成を何度も繰り返さなくて済むように、
番号に対して一度だけ生成し、3連続と5連続で現れた数字を付記した無限リストを作る。

このリストの各要素に対して、それから番号が+1000以内のもので、後半の条件を満たすものがあるかを調べ、
そうであるものだけを抽出する、という流れにする。

5連続で表れた数字を見つけやすいように、見つけた数字を0～15のビットで表す。
3連続で現れた数字についても同じ表現にすれば、ビットANDをとれば一致するかわかる。

## 生成

まずはハッシュの16進表現文字列を取り出す。

```haskell
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as BSL

idxSeq :: String -> [(Int,Word16,Word16)]
idxSeq salt =
  [ (i, c3, c5)
  | i <- [0 ..]
  , let hash = show $ md5 $ BSL.pack $ salt ++ show i
  , ...
```

この中に同じ数字が3つ連続で現れるならその数字を特定する。
ここで、問題文にも書いてあるが**最初のものだけ**であることに注意。
つまり `...333...555...` となったとき、`3`だけに注目し、`5`については考えないということ。
これを見落としてハマった。

`digitToInt` は実は16進数にも対応している。

```haskell
import Data.List.Split
import Data.Word
import Data.Char
import Data.Bits
import Data.List

firstcontinuous3 :: String -> Word16
firstcontinuous3 str =
  foldl' (.|.) 0 $
  map (bit . digitToInt . head) $
  take 1 $
  filter (\(x:xs) -> all (x ==) xs) $
  divvy 3 1 str
```

同様に、同じ数字が5つ連続で現れるものを**全て**記録する。（複数同時にそうなるのも考えづらいが、可能性としてはある。）

```haskell
continuous5 :: String -> Word16
continuous5 fours =
  foldl (.|.) 0 $
  map (bit . digitToInt . head) $
  filter (\(x:xs) -> all (x ==) xs) $
  divvy 5 1 fours
```

`idxSeq` を完成させる。

```haskell
idxSeq :: String -> [(Int,Word16,Word16)]
idxSeq salt =
  [ (i, c3, c5)
  | i <- [0 ..]
  , let hash = show $ md5 $ BSL.pack $ salt ++ show i
  , let c3 = firstcontinuous3 hash, c3 /= 0
  , let c5 = continuous5 hash ]
```

試してみる。

```
ghci> take 10 $ idxSeq "abc"
[(18,256,0),(39,16384,0),(45,8192,0),(64,32,0),(77,32768,0),(79,1024,0),(88,32768,0),(91,1,0),(92,512,0),(110,512,0)]
```

問題文中の記述とも合致しているようだ。

## テスト

`idxSeq` の生成するリストの要素は、全て3つ並びの条件を満たしている。
その中で、続く1000個の中に5つ並びの条件を満たしているものだけを抽出する。

```haskell
genKeys salt =
  [ i
  | (i,c3,_):jdds <- tails $ idxSeq salt
  , any (\(_,_,c5) -> c3 .&. c5 /= 0) $ takeWhile (\(j,_,_) -> j <= i + 1000) jdds ]

sample = "abc"
input = "jlmsuwbz"

test1 = take 64 $ genKeys sample
main1 = take 64 $ genKeys input
```

試してみる。

```
ghci> test1
[39,92,...,22193,22728]
```

# パート2

これはまたえげつない改変。

パート1ではひとつの番号につきMD5の計算は一度だったものを2017回にすると、
単純計算で2017倍の時間がかかるということ。
ハッシュというものの性質から、この問題の計算の範囲で値が重複してキャッシュが効くとも思えない。
つまり、素直にやるしかないということ。

2017回掛ける。

```haskell
md52017 :: String -> String
md52017 str = iterate (show . md5 . BSL.pack) str !! 2017
```

これを使うように `idxSeq` 以上を順次修正する。

```haskell
idxSeq2 :: String -> [(Int,Word16,Word16)]
idxSeq2 salt =
  [ (i, c3, c5)
  | i <- [0 ..]
  , let hash = md52017 $ salt ++ show i       -- ココ
  , let c3 = firstcontinuous3 hash, c3 /= 0
  , let c5 = continuous5 hash
 ]

genKeys2 salt =
  [ i
  | (i,c3,_):jdds <- tails $ idxSeq2 salt
  , any (\(_,_,c5) -> c3 .&. c5 /= 0) $ takeWhile (\(j,_,_) -> j <= i + 1000) jdds ]

test2 = take 64 $ genKeys2 sample
main2 = take 64 $ genKeys2 input
```

しかし重くてあからさまに辛いので、コンパイル実行する。
すると出力がバッファされて観察できないので、`Debug.Trace`を仕込む。

```haskell
import Debug.Trace

genKeys2 salt =
  [ i
  | (i,c3,_):jdds <- tails $ idxSeq2 salt
  , any (\(_,_,c5) -> c3 .&. c5 /= 0) $ takeWhile (\(j,_,_) -> j <= i + 1000) jdds
  , traceShow i True
  ]

test2 = (!! 63) $ genKeys2 sample
main2 = (!! 63) $ genKeys2 input

main = do
  print test2
  print main2
```
