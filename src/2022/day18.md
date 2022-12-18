# 18 日目: 沸騰する岩

（原題 Boiling Boulders で頭韻）

あなたとゾウたちはついに新鮮な空気にたどり着きました。
活発に噴火しているように見える、大きな火山のふもと近くに出ました！
幸いなことに、溶岩はあなたから離れて海に向かって流れているようです。

溶岩のかけらがまだあなたに向かって放出されているので、洞窟の出口にもうしばらく避難していることにします。
洞窟の外では、溶岩が池に流れ着くのが見え、溶岩が固まるシューという大きな音が聞こえます。

溶岩の特定の組成とそれが冷える速度によっては、[黒曜石](https://ja.wikipedia.org/wiki/%E9%BB%92%E6%9B%9C%E7%9F%B3)ができている可能性があります！
冷却速度は溶岩の液滴の表面積に基づいているはずなので、溶岩の飛沫があなたのそばを通り過ぎるときにすばやくスキャンします（パズルの入力）。

溶岩の移動速度が速いため、スキャンはあまりうまくいきません。
その解像度は非常に低く、その結果、**3次元グリッド上の1x1x1の立方体**で溶岩滴の形状を近似し、
それぞれがその`x,y,z`座標として与えられます。

表面積を概算するには、別の立方体に直に接していない各立方体の側面の数を数えます。
したがって、スキャン結果が `1,1,1` と `2,1,1` のような2つの隣接する立方体のみである場合、
立方体はそれぞれ1つの側面が覆われ、5つの側面が露出し、側面の合計表面積は`10`になります。

より大きな例を示します。

```
2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
```

上記の例では、別の立方体に直に接していない全ての側面を数えると、総表面積は**64**です。

**あなたのスキャンでは、溶岩滴の表面積はいくつですか？**

<!--
<details><summary>解説</summary><div>

読み込みは手抜きする。

```haskell
type XYZ = (Int,Int,Int)

parse :: String -> XYZ
parse s = read $ '(' : s ++ ")"
```

それぞれの座標軸について最小値と最大値を取り出し、
その周囲1マスまでを3次元配列で普通にボクセル表現する。

それぞれのマスについて、面で接する上下左右前後の8マスが空虚なら表面積として数え、
他のボクセルがあるときは数えない。

```haskell
import Data.Array

getLH :: (XYZ -> Int) -> [XYZ] -> (Int,Int)
getLH f cs = (pred $ minimum vs, succ $ maximum vs)
  where
    vs = map f cs

neighbors :: XYZ -> [XYZ]
neighbors (x,y,z) = [(pred x,y,z),(x,pred y,z),(x,y,pred z)
                    ,(succ x,y,z),(x,succ y,z),(x,y,succ z)]

compute1 :: [XYZ] -> Int
compute1 xyzs = length
    [() | xyz <- xyzs, xyz1 <- neighbors xyz, not $ arr ! xyz1]
  where
    (xL,xH) = getLH (\(x,_,_) -> x) xyzs
    (yL,yH) = getLH (\(_,y,_) -> y) xyzs
    (zL,zH) = getLH (\(_,_,z) -> z) xyzs
    arr = accumArray (||) False ((xL,yL,zL),(xH,yH,zH)) [(xyz,True) | xyz <- xyzs]

phase1 fn = print . compute1 . map parse . lines =<< readFile fn
```

</div></details>
-->

## パート2

あなたの計算はどこかおかしいようです。
冷却速度が影響するのは外部表面積ですが、
あなたの計算には溶岩滴に封じ込められた気泡の表面積も含まれていました。

代わりに、溶岩の液滴が池に転がり落ちるときに水と蒸気が到達できる立方体の側面だけを考えてください。
蒸気は可能な限り拡大し、溶岩の液滴の外側にある空気を完全に追い出しますが、斜めに拡大することはありません。

上記の大い方の例では、ちょうど1つの立方体の気泡（座標`2,2,5`にある）が溶岩滴内に閉じ込められているため、
溶岩滴の外表面積は**58**です。

**あなたのスキャンでは、溶岩滴の外表面積はいくつですか？**

<!--
<details><summary>解説</summary><div>

`(xL,yL,zL)` は外側であることが保証される。
この位置から水を塗り広げて、溶岩に衝突した回数がすなわち外表面積である。
塗り広げのため、更新のできる命令型配列が必要である。
配列の要素は、0が未処理、1が溶岩、2が水とする。

```haskell
import Data.Array.IO
import Control.Monad

phase2 fn = print =<< compute2 . map parse . lines =<< readFile fn

compute2 :: [XYZ] -> IO Int
compute2 cs =
  do
    arr <- newArray bnds 0 :: IO (IOArray XYZ Int)
    forM_ cs $ \xyz -> writeArray arr xyz 1
    loop 0 arr [(xL,yL,zL)]
  where
    (xL,xH) = getLH (\(x,_,_) -> x) cs
    (yL,yH) = getLH (\(_,y,_) -> y) cs
    (zL,zH) = getLH (\(_,_,z) -> z) cs
    bnds = ((xL,yL,zL),(xH,yH,zH))
```

塗りつぶしは、後で探索するべきマスの位置をスタックで持ちまわす。

```haskell
    loop :: Int -> IOArray XYZ Int -> [XYZ] -> IO Int
    loop cnt _ [] = return cnt
    loop cnt arr (xyz:xyzs) =
      do
        v <- readArray arr xyz
        case v of
          1 -> loop (succ cnt) arr xyzs  -- 溶岩に当たったのでカウント++
          2 -> loop cnt arr xyzs         -- 既に水があるので何もしない
          0 ->                           -- 初めて到達。水が流れ込む
            do
              writeArray arr xyz 2
              loop cnt arr $ filter (inRange bnds) (neighbors xyz) ++ xyzs
```

</div></details>
-->
