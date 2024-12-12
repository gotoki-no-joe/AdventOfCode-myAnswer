切った貼ったがしやすい、indexでのアクセスもできる、そんなデータ構造を木で作るか？

必要な演算
・指定した位置から右または左に、目標の値を順に探してその位置を返す機能、または順に要素を取り出す機能
・splitAtのように切り分ける機能
・(++)のようにつなぐ機能

```haskell
Tree = Nil | Leaf Int | Node Int Tree Tree

-- 補助関数
size (Leaf _) = 1
size (Node k _ _) = k
size Nil = 0 -- は普通は出てこないつもり

-- 一番左を0として、背番号でアクセス
read 0 (Leaf x) = x
read p (Node _ lt rt)
  | q < 0     = read p lt
  | otherwise = read q rt
  where
    q = p - size lt

-- 二つを繋ぐ、バランス調整なし
append Nil rt = rt
append lt Nil = lt
append lt rt = Node (size lt + size rt) lt rt

-- 順に取り出す
flatten t = loop t []
  where
    loop (Leaf x) rest = x : rest
    loop (Node _ lt rt) rest = loop lt $ loop rt rest

-- 切り離す
split 0 t = (Nil, t)
split p (Node _ lt rt)
  | q < 0 = (append lt rt1, rt2)
  | otherwise = (lt1, append lt2 rt2)
  where
    q = p - size lt
    (rt1, rt2) = split q rt
    (lt1, lt2) = split p lt

-- リストからバランス良く作る
list2tree xs = loop (length xs) xs
  where
--    loop 0 _     = Nil
    loop 0 _ = error "zero sized"
    loop 1 (x:_) = Node x
    loop k xs    = Node k (loop k1 xs) (loop (k-k1) $ drop k1 xs)
      where
        k1 = div k 2
```

いけそうな気がしてきた。

動き出したはいいけど、結局がっつり放置するしかないのよん。

appendがO(1)で動く手抜き版なのは、つまり木が高くなって、その他の操作が時間かかる系
なのを改善できないか。

```haskell
Tree = Nil | Leaf Int | Node Int Tree Tree

-- 片方がNilなら捨てる
append Nil t = t
append t Nil = t

-- 両方がLeafなら並べるだけ
append lt@(Leaf _) rt@(Leaf _) = Node 2 lt rt
```

LeafとNodeのとき、ただくっつけるよりは、何か工夫したい。
局所的にバランスすること、とすると、一番下に付けて、それが連続するとCons Listになってしまう。
一旦一番下に付けてから、バランスさせて上まで戻る、という感じか？

```haskell
append (Node k lt rt) l@(Leaf _) = balance $ Node (succ k) lt (append rt l)
append l@(Leaf _) (Node k lt rt) = balance $ Node (succ k) (append l lt) rt
```

`Node` どうしのときは、3通りの回転で、重さの釣り合いが一番とれるものを選ぶ。
どういう基準で？

```haskell
append l@(Node a l1 r1) r@(Node b l2 r2)
  | ??? = Node (a+b) l r
  | ??? = Node (a+b) l1 (Node ? r1 r)
  | ??? = Node (a+b) (Node ? l l2) r2
```

というかこれがbalanceの正体なので、上はもう少し変わってこうなるのな。

```haskell
append (Node k lt rt) l@(Leaf _) = balance lt (append rt l)
append l@(Leaf _) (Node k lt rt) = balance (append l lt) rt
append l@(Node _ _ _) r@(Node _ _ _) = balance l r

balance l@(Node a l1 r1) r@(Node b l2 r2)
  | ??? = Node (a+b) l r               -- (C)
  | ??? = Node (a+b) l1 (Node ? r1 r)  -- (R)
  | ??? = Node (a+b) (Node ? l l2) r2  -- (L)
```

4つの子のsizeをそれぞれ sl1, sr1, sl2, sr2 として、
(L) : sl1 + sr1 + sl2 :             sr2
(C) : sl1 + sr1       :       sl2 + sr2
(R) : sl1             : sr1 + sl2 + sr2
こんな重量配分で、つまり左右の差の絶対値で最小のものを選ぶのがベストだから、
そんな感じに書けばいいんだな。

```haskell
balance l@(Node sl ll lr) r@(Node sr rl rr) =
  snd $ minimumBy (compare `on` fst)
  [ (abs $ sl - sr       , Node (sl + sr) l r)
  , (abs $ sll - slr - sr, Node (sl + sr) ll $ Node (slr + sr) lr r)
  , (abs $ sl + srl - srr, Node (sl + sr) (Node (sl + srl) l rl) rr)]
```

バランスするようにはなったけど、この問題には無力っぽい。

# リンクリスト？

ごっそり移動を高速化するには、リンクリストを使うことが考えられる。
しかし、それはそれで、currentの前の数字、destがどこにあるか線形探索する時間は変わらない。
つまり、並び方としての前後関係と、数の大小関係における前後関係の、両方をリンクリストにせよ？
違う、リンクリストを実装する配列の添え字を、その値だと考えればいいのでは？
そうすれば、必ず手前隣にそれはある、ということになる。どうだ？

せいかい！
