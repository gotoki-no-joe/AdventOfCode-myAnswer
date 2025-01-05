# 入力

1行めは7文字捨ててから数字列として読む。
それ以降は、空行を区切りして行の塊に分けて、
それぞれ先頭行以外を数字列として読む。

これ以上の処理は本体に任せよう。

```haskell
import Data.List.Split

runner i f = do
  sl : ls <- lines <$> readFile i
  let seeds = map read $ words $ drop 7 sl
      mss = map (map (map read . words) . tail) $ wordsBy null ls
  print $ f seeds mss

test1 = runner "saple.txt" part1
main1 = runner "input.txt" part1

part1 :: [Int] -> [[[Int]]] -> Int
part1 seeds mss = ...
```

# パート1

数はどれも0以上のようだ。

それぞれの種は、写像により順に次の自然数に写され、最終的に何らかの自然数になる。
なので種の個数ぶんだけこれを計算して、最小値を求める。

写像のデータは線形探索でも計算は可能だろうが、もう少し気の利いたデータ構造を使いたい。
`IntMap`に、元範囲の開始をキー、先範囲の値に変換するために足し込むためのオフセットを持たせる。

範囲の終わりは、オフセット0の区間の始まりを置くことで表現する。
ここで、区間終わりを書き込むとき、先に別の区間の始まりが既にあるときに上書きしないように注意が必要である。
そのため、区間終わりだけを先に登録した`IntMap`に対して、区間始まりを上書きする順序で構成する。
また、下限である0にも区間終わりを置いておく。

このような`IntMap`に対して`lookupLE`で検索することで、その値にかけるべきオフセットが見つかる。

```haskell
import qualified Data.IntMap as IM

ms2map ms = IM.union opens cloze
  where
    opens = IM.fromList           [(dom, ran - dom) | ran:dom:_ <- ms]
    cloze = IM.fromList $ (0,0) : [(dom + wid, 0)  | _:dom:wid:_ <- ms]

apply im x = x + o
  where
    Just (_,o) = IM.lookupLE x im
```

`mss` に対して `apply` の数珠つなぎを作り、それぞれの種に適用する。

```haskell
part1 seeds mss = minimum $ map applyAll seeds
  where
    applyAll x = foldl (flip ($)) x $ map (apply . ms2map) mss
```

# パート2

サンプルの種の個数は 14 + 13 = 27個、一方本番データのそれは9桁の数が並んでいる。
パート1の計算を愚直に繰り返す方法は使えない。
まだ5日目とは思えない難しさ。

連続した要素を持つ整数集合を、連続区間の両端の集まりで表現する。
より具体的には、区間の下端（含む）にTrue、上端（含まない）にFalseを印つける`IntMap`で表す。

```haskell
import qualified Data.IntMap as IM

newtype SpanSet = SS {getIM :: IM.IntMap Bool}
  deriving Show -- デバッグ用

type Span = (Int,Int)

emptySS :: SpanSet
emptySS = SS IM.empty

-- 集合ssに整数xが含まれるか
isIn :: SpanSet -> Int -> Bool
isIn ss x = maybe False snd $ IM.lookupLE x $ getIM ss
```

`SpanSet` を `type` でなく `newtype` としたのは、`QuickCheck` によるテストのための生成器を付けるため。
まず、周辺関数の実装がそれを前提とする性質を検査する述語：

```haskell
valid :: SpanSet -> Bool
valid (SS is) = even (IM.size is) && and (zipWith (==) (IM.elems is) (cycle [True, False]))
```

中心的な、集合に区間 $[u,v)$ を追加する関数：

```haskell
-- 集合ssに区間[u,v)を追加
addSpan :: SpanSet -> Span -> SpanSet
addSpan ss (u,v) = SS $
  (if isIn ss (pred u) then id else IM.insert u True) $
  (if isIn ss v then id else IM.insert v False) $
  IM.union is1 is2
  where
    (is1,_) = IM.split u $ getIM ss
    (_,is2) = IM.split v $ getIM ss

-- 空集合に区間を入れたら結果は正しい、という性質
prop_addSpan1 (NonNegative x) (Positive w) = valid s1
  where
    s1 = addSpan emptySS (x, x + w)
```

さらに、`SpanSet` の生成器を作ることで、一般の場合のテストができる：

```haskell
instance Arbitrary SpanSet where
  arbitrary = sized $ \n -> do
    xs <- map getNonNegative <$> vectorOf n arbitrary
    ys <- map getPositive <$> vectorOf n arbitrary
    return $ foldl (\ss (x, y) -> addSpan ss (x, x + y)) (SS IM.empty) $ zip xs ys

-- 正しい集合に区間を入れたら結果は正しい
prop_addSpan2 ss (NonNegative x) (Positive w) = valid ss ==> valid (addSpan ss (x, x + w))
```

（こんなものが書いてあるということは、`addSpan` の実装はバグり散らかしたということである。）

ある線より下、上、で集合を切る演算：

```haskell
-- 集合ssのx未満の範囲
under :: SpanSet -> Int -> SpanSet
under ss x
  | isIn ss (pred x) = SS $ IM.insert x False is1
  | otherwise = SS is1
  where
    (is1, _) = IM.split x $ getIM ss

prop_under ss x = valid $ under ss x

-- 集合ssのx以上の範囲
over :: SpanSet -> Int -> SpanSet
over ss x
  | isIn ss x = SS $ IM.insert x True is1
  | otherwise = SS is1
  where
    (_, is1) = IM.split x $ getIM ss

prop_over ss x = valid $ over ss x
```

一通りの道具が準備できたので、本題に入る。

区間に対してこの問題が定義する写像を適用するには、
- それぞれの定義域区間を切り抜き、値域区間に写す
- 切り抜きで残された区間はそのまま値域に写すので、これも含めて
- 全ての値域区間の和集合を作る

とすればできる。
最後に和集合をとることを考えると、写した値域区間は和集合表現にせず、区間のリストで保持する方が都合がよい。

```haskell
import Data.List.Split

-- 集合ssにran dom widな写像を適用する
-- 残されるdom未満の部分およびdom+wid以上の部分と、
-- dom以上dom+wid未満の各区間をranに写した結果のリストを返す
apply2ss :: SpanSet -> [Int] -> (SpanSet, [Span])
apply2ss ss (ran:dom:wid:_) = (rest, uvs)
  where
    domu = dom + wid
    rest = SS $ IM.union (getIM $ under ss dom) (getIM $ over ss domu)
    ofs = ran - dom
    uvs = map l2t $ chunksOf 2 $ map (ofs +) $ IM.keys $ getIM $ flip under domu $ over ss dom

l2t [u,v] = (u,v)

-- ひとつの写像は (ran:dom:wid:_) のリストで構成される
-- これを全て適用する（区間は交差しない前提で考える）
applyMap :: SpanSet -> [[Int]] -> SpanSet
applyMap is0 ms = foldl addSpan isZ $ concat uvss
  where
    (isZ, uvss) = mapAccumL apply2ss is0 ms
```

写像の列を全て順に適用した最終結果の最小値を見つける。

```haskell
part2 :: [Int] -> [[[Int]]] -> Int
part2 seeds mss = x
  where
    is0 = foldl addSpan emptySS $ map (mkSpan . l2t) $ chunksOf 2 seeds
    SS isZ = foldl applyMap is0 mss
    (x,True) = IM.findMin isZ
    mkSpan (x,w) = (x, x + pred w)
```
