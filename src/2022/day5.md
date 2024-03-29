# 5 日目: 物資の山

船から最後の物資が降ろされ次第、遠征隊は出発できます。
物資はそれぞれ印付きの木箱に収められ、積み上げられてスタックをなしていますが、
必要な物資は他の多くの木箱の下に埋もれているため、木箱を再配置する必要があります。

船には、木箱をスタック間で移動できる巨大な貨物クレーンがあります。
木箱がつぶれたり倒れたりしないように、クレーンのオペレーターは慎重に計画された一連の手順で木箱を再配置します。
木箱を再配置すると、目的の木箱が各スタックの一番上になります。

小人たちは、この繊細な手順の間、クレーンオペレーターの邪魔をしたくありませんが、どの木箱がどこに到着するのかを尋ねるのを忘れていました。
出発するために、再配置が完了したらできるだけ早く荷下ろしにかかれるように備えたいと思っています。

しかし彼らは、木箱のスタックの初期状態の図と再配置手順（パズル入力）しか持っていません。
例えば：

```
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
```

この例では、木箱のスタックが3つあります。
スタック1は2つの木箱からなります。木箱`Z`は下にあり、木箱`N`は上にあります。
スタック2は3つの木箱からなります。下から上に、木箱 `M`, `C`, `D` です。
最後に、スタック3は1つの箱`P`からなります。

続きに、再配置手順が示されています。
手順の各ステップで、ある量の木箱が1つのスタックから別のスタックに移動されます。
上記の再配置手順の最初のステップでは、1つの木箱がスタック2からスタック1に移動され、
その結果次の構成になります。

```
[D]        
[N] [C]    
[Z] [M] [P]
 1   2   3 
```

第2ステップでは、3つの木箱がスタック1からスタック3に移動します。
木箱は**一度にひとつ**ずつ運ばれるので、
結果として最初の木箱(`D`)は2つめ3つめの木箱の下に移されます。

```
        [Z]
        [N]
    [C] [D]
    [M] [P]
 1   2   3
```

次に、両方の木箱がスタック2からスタック1に運ばれます。
ここでも木箱は一度に1つずつ移動されるため、木箱`C`は木箱`M`の下になります。

```
        [Z]
        [N]
[M]     [D]
[C]     [P]
 1   2   3
```

最後に、1つの木箱がスタック1からスタック2に移動されます。

```
        [Z]
        [N]
        [D]
[C] [M] [P]
 1   2   3
```

小人たちは、**最後にどの木箱が各スタックの一番上になるか**を知る必要があります。
この例では、一番上の木箱はスタック1では`C`、スタック2は`M`、スタック3は`Z`になため、
これらを組み合わせて、エルフに`CMZ`とメッセージを送る必要があります。

**再配置手順が完了した後、各スタックの一番上にある木箱は何ですか？**

<!--
<details><summary>解説</summary><div>

ファイルを読み込んだら、前半と後半が空行で区切られているので、これで切り分け、空行は除く。

```haskell
(ls1, _:ls2) <- break null . lines <$> readFile "input.txt"
```

前半のスタックの絵は、スタックごとに、上を前にしたリストで取り出したい。
これは `Data.List.transpose` で転置したあと、必要な行だけ抜き出すことでできる。
（一番右のスタックが最大でない場合、行末を揃えるために空白を追加する必要がある。自分は不要だった。）
なお、前半の最後はスタック位置を表すだけで不要なので捨てる。

取り出したスタックの初期状態は、この後の手続き的な更新に備えて、
スタック番号をキーにした `IntMap` に入れておく。

```haskell
let ls1t = transpose $ init ls1
let m0 = IM.fromAscList $ zip [1..] $ map (dropWhile (' ' ==) . (ls1t !!)) [1,5..33]
```

ファイル後半の手順書は、3つの数を抜き出すだけでよい。

```haskell
parse :: String -> (Int,Int,Int)
parse xs = (read w1, read w2, read w3)
  where
    (_:w1:_:w2:_:w3:_) = words xs
```

ひとつの指示(a,b,c)を実行するには、スタックbから要素をひとつずつ取り出してスタックcに乗せることをa回繰り返す。

```haskell
step1 :: IM.IntMap String -> (Int,Int,Int) -> IM.IntMap String
step1 m1 (a,b,c) = IM.insert c imc $ IM.insert b imb m1
  where
    move 0 xs ys = (xs,ys)
    move k (x:xs) ys = move (pred k) xs (x:ys)
    (imb, imc) = move a (m1 IM.! b) (m1 IM.! c)
```

これを全ての行について実行した結果のスタックから、先頭要素を全て順に取り出したものが答えである。

```haskell
body step = do
  (ls1, _:ls2) <- break null . lines <$> readFile "input.txt"
  let ls1t = transpose $ init ls1
  let m0 = IM.fromAscList $ zip [1..] $ map (dropWhile (' ' ==) . (ls1t !!)) [1,5..33]
  let mZ = foldl step m0 $ map parse ls2
  putStrLn $ map head $ IM.elems mZ

main1 = body step1
```

</div></details>
-->

# パート2

クレーンのオペレーターが木枠を巧みに再配置するのを見ていると、
手順があなたの予測とは違った進み方をしていることに気付きます。

クレーンの側面の文字に泥がかぶっていたので、さっと拭き取ります。
クレーンはCrateMover 9000ではありません。これは**CrateMover 9001**です。

CrateMover 9001は、多くの新しくエキサイティングな機能で有名です：
エアコン、革張りのシート、追加のカップホルダー、
そして**複数の木箱を一度に持ち上げて移動する機能**。

上記の例をもう一度考えます。木箱は同じ構成で始まります。

```
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 
```

単一の木箱をスタック2からスタック1に移動すると、以前と同じように動作します。

```
[D]        
[N] [C]    
[Z] [M] [P]
 1   2   3 
```

しかし、スタック1からスタック3に3つの木箱を移動させる動作は、
これらの移動された3つの木箱**順序が同じまま**であることを意味し、次の新しい構成になります。

```
        [D]
        [N]
    [C] [Z]
    [M] [P]
 1   2   3
```

次に、両方の木箱がスタック2からスタック1に移動され、**順序も保持**されます。

```
        [D]
        [N]
[C]     [Z]
[M]     [P]
 1   2   3
```

最後に1つの木箱がスタック1からスタック2に移されます。今回移動されるのは木箱`C`です。

```
        [D]
        [N]
        [Z]
[M] [C] [P]
 1   2   3
```

この例では、CrateMover 9001が木箱をまったく異なる順序 `MCD` に配置しています。

再配置プロセスが完了する前に、シミュレーションを更新して、
小人が最後の物資を降ろす準備をして待つべき場所を把握できるようにします。
**再配置手順が完了した後、各スタックの一番上にある木箱は何ですか？**

<!--
<details><summary>解説</summary><div>

スタックの要素をひとつずつ移動させる代わりに、
指定された個数分だけ一度に、同じ順で移す。

```haskell
step2 :: IM.IntMap String -> (Int,Int,Int) -> IM.IntMap String
step2 m1 (a,b,c) = IM.insert c imc $ IM.insert b imb2 m1
  where
    (imb1, imb2) = splitAt a (m1 IM.! b)
    imc = imb1 ++ m1 IM.! c

main2 = body step2
```

</div></details>
-->
