# 9日目：一晩で

毎年、サンタは、一晩ですべてのプレゼントを配達します。

しかし、今年は新たに訪れる場所がいくつか増えました。
彼の妖精は彼に場所のすべての対の間の距離を与えました。
彼は自分が望む任意の2つの（異なる）場所から出発して終了することができますが、
各場所をちょうど1回ずつ訪問する必要があります。
これを達成するための彼の移動する**最短距離**は何ですか？

たとえば、次の距離が与えられたとします。

~~~
London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141
~~~

したがって、可能なルートは次のとおりです。

~~~
Dublin -> London -> Belfast = 982
London -> Dublin -> Belfast = 605
London -> Belfast -> Dublin = 659
Dublin -> Belfast -> London = 659
Belfast -> Dublin -> London = 605
Belfast -> London -> Dublin = 982
~~~

これらの中で最短のものはLondon -> Dublin -> Belfast = 605で、
この例に対する答は605です。

最短ルートの距離はどれくらいですか？

<details><summary>解説</summary><div>

都市間を飛び回る距離なので、どちら向きにも移動できる無向グラフと見なせる。
都市の数も大した事ないので、都市のリストの順列を生成し、その距離を求める。

まず読み取りを行う。

```haskell
parse :: String -> (String,String,Int)
parse xs = (ws !! 0, ws !! 2, read $ ws !! 4)
  where
    ws = words xs
```

読み取った距離は、都市名を昇順にしたペアをキーに、距離を値とするマップに入れておく。
また、順不同で都市名ペアから距離を取り出す補助関数も定義しておく。

```haskell
import qualified Data.Map as M

-- ccds は parse の結果のリストとする
distMap :: M.Map (String,String) Int
distMap = M.fromList [(minMax c1 c2, d) | (c1,c2,d) <- ccds]

distOf :: String -> String -> Int
distOf c1 c2 = distMap M.! minMax c1 c2

minMax :: Ord a => a -> a -> (a, a)
minMax a b = if a <= b then (a,b) else (b,a)
```

順列は `Data.List.permutations` で生成できるし、自分で作っても大した手間ではない。

```haskell
perms [] = [ [] ]
perms xs = [x:ys | x <- xs, ys <- perms $ delete x xs]
```

順列を作りだす元として、都市名のリストが必要。
重複を `nub` で除去する。

```haskell
import Data.List

cities = nub [c | (c1,c2,_) <- ccds, c <- [c1,c2]]
```

`perms cities` のそれぞれの結果の総距離を求める。

```haskell
fullDist :: [String] -> Int
fullDist cs = sum $ zipWith distOf cs (tail cs)
```

以上の流れを対話環境で実行してもいいし、IOアクションにまとめてもよい。

```haskell
import qualified Data.Map as M
import Data.List

main1 = do
  co <- readFile "input.txt"
  print $ part1 $ lines co

part1 ls = (minimum dps, maximum dps)
  where
    ccds = map parse ls
    distMap = M.fromList [(minMax c1 c2, d) | (c1,c2,d) <- ccds]
    cities = nub [c | (c1,c2,_) <- ccds, c <- [c1,c2]]
    dps = [(fullDist cs, cs) | cs <- permutations cities]
```
</div></details>

# パート2

翌年、見せびらかすために、逆にサンタは**最長距離**の経路を取ることにしました。

前回同様、
彼は自分が望む任意の2つの（異なる）場所から出発して終了することができますが、
各場所をちょうど1回ずつ訪問する必要があります。

例えば、上記の距離を考えると、
最長の経路は例えば Dublin -> London -> Belfastの982です。

最長ルートの距離はどれくらいですか？

<details><summary>解説</summary><div>

パート1でついうっかり最長の経路の距離まで求めてしまったら、
パート2ですることがなくなってしまった。

</div></details>
