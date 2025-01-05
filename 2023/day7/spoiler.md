# 入力

手札は5文字の文字列のまま、ベッドした金額は数値としてリストにする。

```haskell
runner i f = do
  ls <- map parse . lines <$> readFile i
  print $ f ls

parse :: String -> (String, Int)
parse l = (h, read ds)
  where
    h:ds:_ = words l

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 hbs = ...
  where
```

# パート1

カードのラベルを識別符号兼強さの数に対応付ける。2～A に 2～14 を割り当てる。
役で決着が付かなかった場合の比較は、このリストの大小比較になる。

役を判別するために、同じ識別符号をもつカードの枚数を数える。
その結果を大きい順に見たとき

- `[5]` ファイブカード
- `[4,1]` フォーカード
- `[3,2]` フルハウス
- `[3,1,1]` スリーカード
- `[2,2,1]` ツーペア
- `[2,1,1,1]` ワンペア
- `[1,1,1,1,1]` ハイカード（でも勝負はハイカードでは比較しない！）

となるので、これにもスコアを割り振ることで、大小比較できるようにする。

```haskell
import Data.Array
import Data.List

    dic = zip "23456789TJQKA" [2 ..]

    h2is dic h = [i | c <- h, let Just i = lookup c dic]

    cardcnt is =
      sortBy (flip compare) $ elems $
      accumArray (+) 0 (2,14) [(i,1) | i <- is]

    score ks =
      case ks of
        (5:_) -> 7   -- five card
        (4:_) -> 6   -- four card
        (3:2:_) -> 5 -- full house
        (3:_) -> 4   -- three card
        (2:2:_) -> 3 -- two pair
        (2:_) -> 2   -- one pair
        _ -> 1       -- buta
```

弱い順に順位をつけ、これをベッド額に掛けて足し合わせる。

```haskell
part1 hbs =
    sum $ zipWith mul [1 ..] $
    sort [(score $ cardcnt is, is, b) |(h,b) <- hbs, let is = h2is dic h]
  where
    mul r (_,_,b) = r * b
    ...
```

# パート2

カードの識別符号が、`J`が最も小さい値に修正される。

```haskell
    dic2 = zip "J23456789TQKA" [1 ..]
```

`J` のカードをどのように他のカードに読み替えると最善なのか検討する。

- `J`が5枚のとき、これは直ちに最弱のファイブカードとなる。
- `J`が4枚のとき、唯一の異なるカードのファイブカードとみなすのが最善
- `J`が3枚のとき、残りの2枚は 1,1 と異なるか 2 と同じカードかの場合がある。
  - 1,1のとき、ファイブカード以外の任意の役が作れるので、フォーカードとみなすのが最善
  - 2のとき、ファイブカードとみなせる
- `J`が2枚のとき、残りのカードが
  - 3のとき、ファイブカード
  - 2,1のとき、フォーカード
  - 1,1,1のとき、ツーペアよりスリーカード
- `J`が1枚のとき、残りのカードが
  - 4のとき、ファイブカード
  - 3,1のとき、フォーカート
  - 2,2のとき、フルハウス
  - 2,1,1のとき、ツーペアよりスリーカード
  - 1,1,1,1のとき、ワンペア

いずれにせよ、役を作るために特に工夫をすることなく、最も枚数が多いカードに読み替えて役を判断すればよい、とわかった。

```haskell
    cardcnt is = cj+k1 : ks
      where
        k1:ks   = sortBy (flip compare) cnts
        cj:cnts = elems $ accumArray (+) 0 (1,13) [(i,1) | i <- is]
```

残りの定義は共用できるのでグローバルに出すか、むしろ `dic` と `cardcnt` の定義だけテンプレートメソッド的に差し替える `part1` と `part2` 共通の本体関数を立ててもいいぐらい。
