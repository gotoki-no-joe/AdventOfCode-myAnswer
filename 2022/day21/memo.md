ていうかPart2は逆問題かいな。
線形探索で見つかるような数ではないんだろうなぁ。

humnが直接、複数回参照されてはいなさそう。なので一本道っぽい。いちおう。

変数が一つだけの数式処理システムを書くことができる？

```haskell
Value = Num Int | Humn | Add Value Value | Sub Value Value | Mul Value Value | Div Value Value

eval (Num x) = Num x
eval Humn = Humn
eval (Add (Num x) (Num y)) = Num $ x + y
eval ()
```

つらいな。

Part1のシステムを流用して、rootの2つの項のうち、humnが絡まない方の値を求める。
humnが絡む方は、rootの値をその値として、参照の向きを逆にして、humnに降りていくようにできないか。
しかし一方で、humnに降りない側は上らないのいけないのでややこしいね。
どちらがhumnにのぼる向きなのかを、特定しておかなければならない。
初めからすべてgraphで考えたらいい？

ddd : x は、そういう値がラベルされたノード。
aaa : bbb (op) ccc は、違う方から来たらそれなりに解釈するノード。
aaa から来たら、bbbとcccをopすればいいが、bbbから来たらaaa (op)^-1 cccして解釈する。みたいな。
それをhumnを根として評価を回せば、humnの値が確かに得られそう。

ただしrootをどうあつかう。
humnから降りて行ったら、rootに突き当たる。そこから先は全て普通の向きに解釈できてラッキー。
その結果をrootは、「私はこの値です」と持ってきていいのか。

root: aaaa = bbbb
bbbb: 100
aaaa: humn * 10

は、humn = aaaa / 10 = bbbb / 10 = 100 と考えるしかないものな。
あと、ここで見るように、逆計算をするとき、乗算と除算にゆらぎがある。

a = b * 10 => b = a / 10 ただし a mod 10 /= 0 のときは矛盾、ではねる必要がある。
10 = b * a => b = 10 / a -- root 以外でこのような折り返しはないはず、で無視していい？
a = b / 10 => b = a * 10 -- +0～+9 が振り幅として許される。答えが一つでない。
a = 10 / b => b = 10 / a -- これの振れ幅は？うーん。

そんな「細かいこと」を考えなくても行ける問題だと信じて解くとするなら、
入力からグラフの辺を作って、このとき辺について、ある辺を親としてそのノードに到達するとき、
他の2辺をどう解釈するかの計算式を全て貼り付けておいて、
humnからはそれを辿って計算する、という形か。

それようは、rootからやってきた場合か、葉の方から来た場合か、という意味か。
part1ではrootからの解釈しかしなかった。
part2では、humnから始まる、葉の方から見た解釈を全方位に展開する。
ならば、二つのmapあるいはキーをダブらせたmapを作ることになるのな。

上と下だけでなくて、左と右の部分木で扱いが違うから3方向だわ。

```haskell

data Dir = FOR | LST | RST deriving (Eq,Ord)
type Key = (String, Dir)

parse2 :: String -> [(Key, M.Map Key Int -> Int)]
parse2 l =
  case words l of
    [w1,w2] -> [((init w1, FOR), const $ read w2)] -- 定数のとき、上から見る他に見方はない
    [w1,w2,op,w3] -> exprRel (init w1) w2 op w3
    _ -> error "never"

exprRel key w2 "+" w3 = [((key, FOR), \m -> m M.! (w2,FOR) + m M.! (w3,FOR))
                        ,((key, LST), \m -> m M.! (key,))
]

exprRel _ _ _ _ = error "never"
```

ひとつのノードについて、上から来たとき、左下から来たとき、右下から来たとき、で違う計算をするように仕込めばいいかと思ったが、
下から来たとき、もう一方は反対側の子に上から行くだけなのだけど、上に聞きに行くとき、右か左かをこのノードは知らない！
ということに気づいた。どうしたらいいのだ。

どういうノードから訪問していたか、というノード名を持ちまわすか？
何とかなりそうではあるが。

いやそもそも、上に行くとき、誰に聞いていいかもわからん。ローカルでは。
なので、まずグラフを作る、なんだな。

```haskell

type Parsed = Either (String, Int) (String, String, String, String)

parse :: String -> Parsed
parse l =
  case words l of
    [w1,w2] -> Left (init w1, read w2)
    [w1,w2,op,w3] -> Right (init w1, w2, op, w3)
    _ -> error "never"
```

そうでもない。名前を常に付けて尋ねて、左や右の名前ならそうだとわかり、
どちらでもなければ、上からだと判断できるってわけだ。

なくない。
humnすら、その値を誰に聞いていいのかわからない。

なんだろう。

aaaa: bbbb * cccc
という行から、

aaaa := bbbb * cccc
bbbb := [aaaa] / cccc
cccc := aaaa / [bbbb]
という3つの関係を作るということか。
そうか。

a: b + c -> a = b + c, b = a - c, c = a - b
a: b - c -> a = b - c, b = a + c, c = b - a
a: b * c -> a = b * c, b = a / c, c = a / b
a: b / c -> a = b / c, b = a * c, c = b / a
