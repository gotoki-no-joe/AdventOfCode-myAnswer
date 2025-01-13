# 入力

また冗長な書式。トナカイの名前は関係ないので、パラメータだけ抜き出す。

```haskell
parse :: String -> [Int]
parse xs = map (read . (ws !!)) [3, 6, 13]
  where
    ws = words xs

runner i f = readFile i >>= print . f . map parse . lines
```

# パート1

`replicate` と `cycle` で毎秒ごとの飛行距離を作って足し合わせるだけでも答えは出せる。

```haskell
part1 time ls = maximum $ map dist ls
  where
    dist [s,t,u] = sum $ take time $ cycle (replicate t s ++ replicate u 0)

test1 = runner "test.txt"  (part1 1000)
main1 = runner "input.txt" (part1 2503)
```

もう少し気の利いた計算をするなら、
(速度 \\(s\\)、飛行時間 \\(t\\) 秒、休憩時間 \\(u\\)) 秒というトナカイが \\(D\\) 秒飛ぶとき、
1サイクルが \\(t + u\\) 秒かかるので、\\(D \div (t + u) = q \dots r\\) として、
サイクルを \\(q\\) 回行い、さらに最後のサイクルのうち \\(r\\) 秒だけを行う。

完全に行えたサイクルでは $t$ 秒間飛行し、最後の回は $\min(t, r)$ 秒だけ飛行しているので、
移動できた総距離は \\s (q t + \min(t, r)\\)となる。

```haskell
    dist [s,t,u] = s * (q * t + min t r)
      where
        (q,r) = divMod time (t + u)
```

# パート2

こちらは、毎秒ごとの状況を分析する必要があるので、パート1の素朴な方法をベースに考える。

それぞれのトナカイについて、次の1秒で進む距離（または0）のリストを作り、
これを累積することで各時刻の位置を作り、一位を選んで点数を付与する、を繰り返す。

```haskell
import Data.List

part2 time ls =
    maximum $             -- 最高得点
    map sum $ transpose $ -- トナカイごとに足し合わせ
    map point2top $       -- 首位にポイント付与
    take time $           -- レース時間で打ち切り
    transpose $           -- 毎秒ごとに、各トナカイの位置のリスト、のリスト
    map genDists ls       -- トナカイごとに、毎秒の位置のリスト、のリスト
  where
    genDists [s,t,u] = scanl1 (+) $ cycle (replicate t s ++ replicate u 0)
    point2top ds = [if d == m then 1 else 0 | d <- ds]
      where
        m = maximum ds
```
