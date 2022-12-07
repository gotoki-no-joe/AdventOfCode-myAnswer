# 5日目：彼はこれ用の小人研修生を持っていないのですか？

(intern-elvesが何の駄洒落なのかわからない)

サンタは、テキストファイルの文字列がそれぞれ、
「いい」か「いやらしい」のどちらなのかを突き止める手助けを必要としています。

**いい文字列**とは、以下の性質のすべてを有するものです。

- 少なくとも3つの母音（`aeiou`のみ）が含まれている。
例えば`aei`, `xazegov`, `aeiouaeiouaeiou`
- 少なくとも1つ、2回続けて現れる文字を含む。
例えば`xx`, `abcdde` (dd), `aabbccdd` (aa, bb, cc, dd)
- `ab`, `cd`, `pq`, `xy`を**含まない**。
それらが他の要件のいずれかの一部であっても例外ではない。

いい文字列でない文字列は、いやらしい文字列です。

例えば：

- `ugknbfddgicrmopn` はいい文字列です。
少なくとも3つの母音 (u...i...o...)と
続き文字 (...dd...)を含み、
許可されていない部分文字列を持っていません。
- `aaa` はいい文字列です。
少なくとも3つの母音と2つの文字を持つためです。
異なる規則で使用されている文字が重なっていてもかまいません。
- `jchzalrnumimnmhp` は続き文字がないのでいやらしい文字列です。
- `haegwjzuvuyypxyu` は文字列`xy`を含んでいるのでいやらしい文字列です。
- `dvszwmarrgswjxmb` は1つの母音しか含まれていないので、いやらしい文字列です。

いい文字列はいくつありますか？

<details><summary>解説</summary><div>

いい文字列を判定する述語を作る。

「母音を3つ」は、`aaa` の例でもわかるように、同じ文字が3つでもよいので、数えればよい。

```haskell
isVowel x = elem x "aeiou"

cond1 xs = 3 < length (filter isVowel xs)
```

あと2つの条件のために、連続する2文字の全ての組を作っておく。

```haskell
xys = zip xs (tail xs)
```

「続き文字を含む」とは、`xys` の中に、左右が等しいものがあるということになる。

```haskell
cond2 xys = any (uncurry (==)) xys
```

特定の対を含まないとは、そのいずれも `xys` の中に現れないことである。

```haskell
cond3 xys = all (\bad -> notElem bad xys) [('a','b'),('c','d'),('p','q'),('x','y')]
```

入力から、これらすべてを満たす語の数を数える。

```haskell
part1 :: [String]  -- 入力
      -> Int       -- 答え
part1 = length . filter cond123

cond123 xs = cond1 xs && cond2 xys && cond3 xys
  where
    xys = zip xs (tail xs)
```

</div></details>

# パート2

サンタは自分のやり方の間違いに気がついて、
文字列が「いい」か「いやらしい」かを判断するより良いモデルに切り替えました。
古い規則はすべて明らかにばかげているので適用されません。

さて、いい文字列は、以下のすべての性質を持つものです。

- 何らかの2つの文字の対が重ならずに二回またはそれ以上に現れます。
例えば`xyxy` (`xy`)や`aabcdefgaa` (`aa`)はこれにあてはまりますが、
`aaa`は`aa`が2度現れますが重なっているので違います。
- 間にちょうど1文字を挟んで同じ文字が繰り返すような並びが1回またはそれ以上に現れます。
例えば`xyx`, `abcdefeghi` (`efe`) のようなものです。
また`aaa`も該当します。

例えば：

- `qjhvhtzxzqqjkmpb`はいい文字列です。
2回出現する対`qj`と、1文字置いて繰り返される文字`zxz`があるためです。
- `xxyxx`はいい文字列です。
二重に現れる対と間に1文字置いて繰り返される文字があります。
両規則で使用されている文字が重なっていても構いません。
- `uurcxstgmygtbstg`
これには対(`tg`)はありますが、一文字置いた繰り返しがありませんので、いやらしい文字列です。
- `ieodomkazucvgmuy`
1文字置いた繰り返し文字がありますが(`odo`)、2回現れる対はありませんので、いやらしい文字列です。

これらの新しい規則の下で、いい文字列はいくつありますか？

<details><summary>解説</summary><div>

前者の条件は、パート1の `xys` で考えると、直後の対は重なりがあるので、それを除いた以降に等しいものがあればよい。
全ての対について調べ、そのようなものが一つあればよい。

```haskell
cond4 xs = any sub $ tails xys
  where
    xys = zip xs (tail xs)
    sub (xy : _ : xys) = elem xy xys
    sub _ = False
```

後者の条件は、`xys` と同様に対になる文字について判定すればよい。

```haskell
cond5 xs = or $ zipWith (==) xs (drop 2 xs)
```

最後にまとめる。

```haskell
part2 :: [String]  -- 入力
      -> Int       -- 答え
part2 = length . filter cond5 . filter cond4
```

</div></details>
