# 入力

行ごとのデータをレコードで表現する。

```haskell
import Data.List.Split

data Entry = Entry {names :: [String], sect :: Int, checkSum :: String}

parse :: String -> Entry
parse l = Entry { names = as, sect = read sect, checkSum = check }
  where
    ws = endByOneOf "-[]" l
    (as,[sect, check]) = splitAt (length ws - 2) ws
```

`Entry` のリストを読み込んで渡す。

```haskell
runner i f = readFile i >>= print . f . map parse . lines
```

# パート1

`names` からチェックサムを再構成し、与えられたものと等しいか確認する。
文字ごとの個数を数え、頻度の降順、文字の昇順で前から5つ取り出すとチェックサムになる。

```haskell
import Data.Array
import Data.List
import Data.Tuple

valid :: Entry -> Bool
valid e = checkSum e == check1
  where
    cnt :: Array Char Int
    cnt = accumArray (+) 0 ('a','z') [(x, -1) | x <- concat $ names e]
    check1 = take 5 . map snd . sort . map swap . filter ((0 /=) . snd) $ assocs cnt
```

`valid` なものの `sect` を合計する。

```haskell
part1 :: [Entry] -> Int
part1 = sum . map sect . filter valid

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1
```

```
ghci> test1
1514
```

# パート2

それシーザー暗号…

`valid`なものの個数を数えてみる。

```
ghci> runner "input.txt" (length . filter valid)
749
```

結構ある。

ともかく、復号するには、
文字を0から25に置き換え、部屋番号を足してmod 26して、元に戻す。

`Array`で独自の表を作ってもできる

```haskell
i2a = listArray (0,25) ['a' .. 'z']
a2i = listArray ('a','z') [0 .. 25]
```

けれど、まぁ普通は `Data.Char` を使う。

```haskell
import Data.Char

decode e = (unwords $ map (map (i2a . (sect e +) . a2i)) $ names e, sect e)
  where
    orda = ord 'a'
    a2i c = ord c - orda
    i2a i = chr $ mod i 26 + orda
```

試してみる。

```
ghci> decode $ parse "qzmt-zixmtkozy-ivhz-343[]"
("very encrypted name",343)
ghci> runner "input.txt" (take 3 . map decode . filter valid)
[("top secret chocolate management",377)
,("top secret cryogenic plastic grass shipping",422)
,("international bunny sales",227)]
```

全部意味のある言葉にちゃんとなってて、その750個の中から何を見つけろというのかが不明瞭。
問題文

> What is the sector ID of the room where <u>North Pole objects</u> are stored?

で、部屋名は全て小文字であることから、`object` を探してみる。

```haskell
part2 q = filter (isInfixOf q . fst) . map decode . filter valid

main2 q = runner "input.txt" (part2 q)
```

```
ghci> main2a "object"
[("northpole object storage",991)]
```

任務完了。
