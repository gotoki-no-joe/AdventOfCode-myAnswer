# 12日目：JSAbacusFramework.io

サンタの会計担当妖精は、最近の注文後の帳簿のバランスを取るのに手助けか必要です。
残念なことに、彼らの会計ソフトウェアは異様な記録形式を使用しています。
そんなところにあなたが来ました。

彼らはJSON文書を持っています。
それは様々なものを含んでいます。
配列(`[1,2,3]`)、オブジェクト(`{"a":1, "b":2}`)、数値、文字列です。
あなたの最初の仕事は、
文書全体からすべての数値を見つけてそれらを足し合わせることです。

例えば：

- `[1,2,3]`と`{"a":2,"b":4}`は両方とも合計6です。
- `[[[3]]]`と`{"a":{"b":4},"c":-1}`は両方とも合計3です。
- `{"a":[-1,1]}`と`[-1,{"a":1}]`は両方とも合計0です。
- `[]`と`{}`は両方とも合計0です。

文字列に数字が含まれることはありません。

文書内の**すべての数値**の合計はいくつですか？

<details><summary>解説</summary><div>

JSONライブラリを使ってもいいが、この程度の構文は自分で読み込んでしまおう。
まず、JSONデータとは、数字列、ダブルクオートで囲まれた文字列、角括弧とコンマによる配列、
波括弧と文字列とコロンとコンマによるオブジェクトである。

```haskell
import Text.Parsec
import Text.Parsec.Char

data JSON = JSONNum Int | JSONStr String | JSONList [JSON] | JSONObj [(String, JSON)] deriving Eq

numP :: Parsec String u JSON
numP = do
  s <- option id (negate <$ char '-')
  JSONNum . s . read <$> many1 digit

strP :: Parsec String u JSON
strP = JSONStr <$> between (char '"') (char '"') (many1 letter)

lstP :: Parsec String u JSON
lstP = JSONList <$> between (char '[') (char ']') (sepBy jsonP (char ','))

objP :: Parsec String u JSON
objP = JSONObj <$> between (char '{') (char '}') (sepBy colonedP (char ','))
  where
    colonedP = do
      k <- between (char '"') (char '"') (many1 letter)
      char ':'
      v <- jsonP
      return (k,v)

jsonP :: Parsec String u JSON
jsonP = choice [try numP, try strP, try lstP, objP]

allP :: Parsec String u JSON
allP = do
  j <- jsonP
  eof
  return j
```

`JSON`型の値が読み込めたら、走査して数値をすべて列挙し、それを足し合わせる。

```haskell
traverse1 :: JSON -> [Int]
traverse1 j = iter j []
  where
    iter (JSONNum n) rest = n : rest
    iter (JSONList js) rest = foldr iter rest js
    iter (JSONObj kvs) rest = foldr (iter . snd) rest kvs
    iter _ rest = rest

part1 = do
  co <- readFile "input.txt"
  let Right j = runParser allP () "" co
  print $ sum $ traverse1 j
```

</div></details>

# パート2

あちゃー…会計担当妖精は、彼らは赤字をすべて二重に数えたことに気付いた。

値`"red"`を持つ任意のプロパティを持つオブジェクト（およびそのすべての子）を無視します。
これはオブジェクト(`{...}`)だけで行い、配列(`[...]`)では行いません。

- `[1,2,3]`はやはり合計6です。
- `[1,{"c":"red","b":2},3]`は中間のオブジェクトが無視されるため、今は合計4です。
- `{"d":"red","e":[1,2,3,4],"f":5}`は構造体全体が無視されるため、今は合計0です。
- `[1,"red",5]`は合計6です。配列中の`"red"`には何の効果もありません。

<details><summary>解説</summary><div>

走査の際に、オブジェクトに遭遇した場合、値に `"red"` があれば0で戻るロジックを追加する。

```haskell
traverse2 :: JSON -> [Int]
traverse2 j = iter j []
  where
    iter (JSONNum n) rest = n : rest
    iter (JSONList js) rest = foldr iter rest js
    iter (JSONObj kvs) rest
      | elem (JSONStr "red") vs = rest
      | otherwise               = foldr iter rest vs
      where
        vs = map snd kvs
    iter _ rest = rest
```

</div></details>
