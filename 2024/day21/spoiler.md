# ざんげ

このまま未解決で放置するのもひっかかるので、MS copilot さんにヒントを聞いたら
[Todd Ginsbergさんのblog](https://todd.ginsberg.com/post/advent-of-code/2024/day21/)を紹介されて、
読んだら解けたので、独力ではなく受け売りです。

# 入力

めちゃ短い文字列なのでソースに埋め込んでしまってもいいくらいだし、なんにせよ文字列のリストにするだけ。

```haskell
runner i f = do
  ls <- lines <$> readFile i
  print $ f ls

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 ls = ...
```

ここに戻ってくるのはかなり後になる。

# ボトムアップで

受け売り元にならい、ボトムアップに構築していくが、どうしてそういう設計になるのか、という面については、
全体の方針からトップダウンに決まることで、そこが繋がるまでモヤモヤする話の流れであることを覚悟してほしい。

まず、テンキーパッドと方向キーパッドを、どの座標位置にどんなボタンがあるのかという写像で表現する。

```haskell
import qualified Data.Map as M
import Data.List.Split

type Pad = M.Map (Int,Int) Char

numericPad, directionalPad :: Pad
numericPad = makeKeypad "789456123*0A"
directionalPad = makeKeypad "*^A<v>"

makeKeypad :: String -> Pad
makeKeypad str = M.fromList
  [ ((i,j),c)
  | (i, cs) <- zip [0 ..] $ chunksOf 3 str
  , (j, c ) <- zip [0 ..] cs, c /= '*' ]
```

キーパッドを操作するロボを操作する操作系列を求めるとき、
押したいボタンXがどれかだけではなく、現在アームが指しているボタンがどれなのか、が結果に影響する。

そこで、キーパッドの任意の位置から任意の位置へ、ボタンの上だけを辿って到達するための最短の操作系列を、
出発の文字から目的の文字の対に対して割り当てる。最短の操作系列は複数あるので全て持っておく。

まずユーティリティ関数の `shortests` は、リストのリストから、最短のものを**全て**抽出する。
全て欲しいので `minimumBy` ではできない。
ただし、候補がなく空リストのときは空リストを返す。

```haskell
shortests [] = []
shortests xs = [x | (l,x) <- zip ls xs, l == lmin]
  where
    ls = map length xs
    lmin = minimum ls
```

`closer s e` は、
座標 s から座標 e までの無駄のない経路で、e の一歩手前 n と、
n から e に移動するためにカーソルキーで押すボタン文字との対を、
縦移動と横移動の両方について求める。

```haskell
closer :: (Int,Int) -> (Int,Int) -> [((Int,Int),Char)]
closer (i,j) (p,q) =
  [((pred p, q), 'v') | i < p] ++ [((succ p, q), '^') | p < i] ++
  [((p, pred q), '>') | j < q] ++ [((p, succ q), '<') | q < j]
```

作りたいものは、二つのキーパッドについて、その始点文字から終点文字の全ての対について、
最短操作系列を持つようなマップ。

```haskell
type Paths = M.Map (Char,Char) [String]

numericPaths, directionalPaths :: Paths
numericPaths     = allPaths numericPad
directionalPaths = allPaths directionalPad

allPaths :: Pad -> Paths
allPaths pad = ...
  where
    ....
```

一旦、`Pad` のキー、すなわち座標 `(Int,Int)` の形の始点から終点に対する最短系列のマップ `m` を作る。
リスト操作の都合上、順序も反転した形で蓄積する。
遅延`Map`によるDPで、Kotlin版のようなBFSの手続きを書くことなく達成。

```haskell
    m = M.fromDistinctAscList
      [((s,e), findLowestCostPaths s e) -- 最短操作系列を（逆順で）求める
      | s <- M.keys pad                 -- sとeのキーの座標に関して全ての組み合わせで
      , e <- M.keys pad ]
    findLowestCostPaths s e             -- 出発点s終点e
      | s == e = [""]                   -- s = e なら操作はε
      | otherwise = shortests           -- 最短のもの全て
        [ d : p                         -- nからeへの1操作を追加したもの
        | (n,d) <- closer s e           -- eよりsに一歩近い隣nの
        , M.member n pad                -- ただしnは実在するもの限定で
        , p <- m M.! (s,n)]             -- sからnへの最短系列に
```

`m`ができたら、座標からその位置のボタン文字は`pad`から取り出せる。
操作系列も全て`reverse`し、末尾に`A`を追加して完成。

```haskell
allPaths pad =
    M.mapKeys (\(ij, pq) -> (pad M.! ij, pad M.! pq)) $
    M.map (map (reverse . ('A' :))) m
```

確認しておこう。

```
ghci> numericPaths
fromList [(('0','0'),["A"]),(('0','1'),["^<A"]),(('0','2'),["^A"]),(('0','3'),[">^A","^>A"]),...
```

# パート1

ドアの前のロボにコードを入力させるには、アームの初期位置は `A` なので、
例えば `029A` なら `('A','0'), ('0','2'),('2','9'),('9','A')` の
`numericPaths` のそれぞれいずれかを連結したものを与えればよい。

これを操作するロボにそのようなコードを最適に入力させるには、
上で作れるような全ての系列について同様に系列を求め、その最短を選ぶことになる。

これを操作するロボに…以下同文で、これを実際に作ってみよう。

ここで多分、一つの文字の入力の末尾に必ず`A`を押すことで位置をリセットすることが効いていて、
上のロボの系列の全ての組み合わせを下のロボで試す必要はなく、
次のボタンを押すまでの系列ごとに試すだけで済むようになっているのだろう。
（プレイヤー側は「だろう」で済むが、出題側は証明または検証しないといけないので大変だ…）

```haskell
import Data.List
import Data.Function

makePath :: String -> String
makePath code = findCode3 code
  where
    findCode findCodePrev path code = concat
      [ shortest (map findCodePrev p)
      | ab <- zip ('A':code) code
      , let p = path M.! ab ]
    findCodeHead code = code -- 自分は必要なコードをそのまま打ち込む
    findCode1 = findCode findCodeHead directionalPaths
    findCode2 = findCode findCode1    directionalPaths
    findCode3 = findCode findCode2    numericPaths

shortest xs = minimumBy (compare `on` length) xs
```

（`p` を作るところ、`ab` を経由せずに `p <- zipWith (uncurry (path M.!)) ...` にしろと
コードアシストは言うけれど、`uncurry` 使ってまですることではないのでは？という。）

下請けロボの動作は単なる関数呼び出しの合成でなく、中で何度も使うための関数引数として渡す必要がある。
結果も大丈夫そうなので、答えを算出する。

```haskell
part1 ls = sum [length (makePath l) * read (init l) | l <- ls]
```

# パート2

パート1をそのまま拡大するのでは計算機の能力からあふれるだろう。
まず、`findCode` は、下請けへの要求としては操作系列を渡す必要があるが、
戻り値として完成した操作系列を戻す必要はない。最終的に必要なのは長さだけなので。

また、`findCode` の実例を手で作って入れ子を手で組み立てる代わりに、
ロボの段数を引数にして、各自で入れ子を作らせる。
段数0は上の `findCodeHead` に対応する。
最上段だけが `numericPaths` を使い、他は全員 `directionalPaths` を使う。
なので `path` は引数から外すことができる。

```haskell
calcPathLen :: Int -> String -> Int
calcPathLen sysDepth code = findCodeLen sysDepth code
  where
    findCodeLen 0     code = length code -- 自分
    findCodeLen depth code = sum
      [ minimum (map (findCodeLen (pred depth)) p)
      | ab <- zip ('A':code) code
      , let p = path M.! ab ]
      where
        path | depth == sysDepth = numericPaths
             | otherwise         = directionalPaths

part1a d ls = sum [calcPathLen d l * read (init l) | l <- ls]
```

試してみる。

```
ghci> runner "sample.txt" (part1a 3)
126384
ghci> [(d, calcPathLen d "029A") | d <- [3 .. 26]]
[(3,68),(4,164),(5,404),(6,998),(7,2482),(8,6166),(9,15340),(10,38154),(11,Interrupted.
```

26段は無理。

## メモ化

`findCodeLen` は、それぞれの層で、あるキーからあるキーへの移動と`A`を押す、の短い列について計算するだけなので、
最終的な系列が長大になっても、それぞれの層が扱う内容はそれほど複雑化していないはず。
つまり、実は同じ引数に対する計算を繰り返していて、メモ化で効率化できる。

引数が二つあるので `uncurry` して、[集めるDPについて](https://qiita.com/gotoki_no_joe/items/713af82bfd0a94e8a335#%E9%85%8D%E5%88%97%E3%82%92%E5%BC%B5%E3%82%8C%E3%81%AA%E3%81%84%E7%A9%BA%E9%96%93%E3%81%A7%E3%82%82%E9%9B%86%E3%82%81%E3%82%8Bdp) で作ったこれ：

```haskell
memoize :: Ord t => ((t -> s) -> t -> ([t], s)) -> t -> s
memoize fya x = m M.! x
  where
    m = loop M.empty (S.singleton x)
    loop old new
      | S.null new = old
      | otherwise  = loop old1 new1
      where
        (kvs, jss) = unzip [((k,v),js) | k <- S.elems new, let (js, v) = fya (m M.!) k]
        old1 = M.union old $ M.fromList kvs
        new1 = S.fromList $ concatMap (filter (flip M.notMember old1)) jss
```

でメモ化する。

```haskell
calcPathLenM :: Int -> String -> Int
calcPathLenM sysDepth code = memoize findCodeLen (sysDepth, code)
  where
    findCodeLen _     (0    , code) = ([], length code)
    findCodeLen recur (depth, code) = (concat recargss, result)
      where
        ps = map (path M.!) $ zip ('A':code) code
        recargss = [[(pred depth, e) | e <- p] | p <- ps]
        result = sum [minimum (map recur recargs) | recargs <- recargss]
        path | depth == sysDepth = numericPaths
             | otherwise         = directionalPaths

part1b d ls = sum [calcPathLenM d l * read (init l) | l <- ls]
```

5つのコードについて計算するそれぞれについてメモ化がやり直しになるのかどうかが気に掛かるが…

```
ghci> runner "sample.txt" (part1b 3)
126384
ghci> [(d, calcPathLenM d "029A") | d <- [3 .. 26]]
[(3,68),(4,164),(5,404),(6,998),(7,2482),(8,6166),(9,15340),(10,38154),(11,94910),(12,236104),(13,587312),(14,1461046),(15,3634472),(16,9041286),(17,22491236),(18,55949852),(19,139182252),(20,346233228),(21,861298954),(22,2142588658),(23,5329959430),(24,13258941912),(25,32983284966),(26,82050061710)]
```

実行は一瞬でいけちゃった。

```haskell
main2 = runner "input.txt" (part1b 26)
```

できた…！

## 禁断の技

上の `memoize` は行儀はよいが、やはり、可能性のある引数を計算の本体とは別に構築する所が手間で、
そこで計算の二度手間を無くそうとした `calcPathLenM` の `findCodeLen` は
オリジナルと同じ計算をしているようには直観的には見えなくなってしまっている。

書き換え可能な変数を命令型プログラミングのように使って、本当のメモ化をする
[禁断の技](https://qiita.com/gotoki_no_joe/items/713af82bfd0a94e8a335#%E8%BF%BD%E8%A8%98unsafeperformio)を使った版も試しておこう。

```haskell
import Data.IORef
import qualified Data.Map as M
import System.IO.Unsafe

memoizeU :: Ord d => ((d -> a) -> (d -> a)) -> (d -> a)
memoizeU mf = f
  where
    memo = unsafePerformIO $ newIORef M.empty
    f x = unsafePerformIO $ do
      m <- readIORef memo
      case M.lookup x m of
        Just a -> return a
        Nothing -> do
          let a = mf f x
          modifyIORef' memo (M.insert x a)
          return a
```

これを使った `calcPathLenU` の実装は、元の `calcPathLen` とほとんど変わらない。
引数が `uncurry` されたせいで、中間変数 `e` が入ったくらい。

```haskell
calcPathLenU :: Int -> String -> Int
calcPathLenU sysDepth code = memoizeU findCodeLen (sysDepth, code)
  where
    findCodeLen _     (0    , code) = length code
    findCodeLen recur (depth, code) = sum
      [ minimum [recur (pred depth, e) | e <- p]
      | ab <- zip ('A':code) code
      , let p = path M.! ab ]
      where
        path | depth == sysDepth = numericPaths
             | otherwise         = directionalPaths

part1c d ls = sum [calcPathLenU d l * read (init l) | l <- ls]

main2U = runner "input.txt" (part1c 26)
```

以上。
最初はこのメモ化は禁断の技でしかできないと思っていたけれど、immutable な方でも十分だったのが驚き。
（問題の方の性質かもしれないけど。）
