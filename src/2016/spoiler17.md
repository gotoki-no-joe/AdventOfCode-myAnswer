# パート1

同じマスに到達したとしても経路が異なれば扉の状態は違うものになるので、
要素の衝突を全く気にしないでよい幅優先探索を行い、
金庫室まで到達する経路を求める。
MD5ハッシュの内容だけでなく、扉のない方向には行けないことも検出する必要がある。

初期状態から始めて、各ステップでゴールした場合その結果のリスト、
ゴールしていない状態についてはそのリストをシステムの状態として計算を継続する。

```haskell
import Data.List

findPaths passcode = unfoldr step [((0,0), BSL.pack passcode)]
  where
-- 座標に基づくドアの確認
    doors (i,j) = [i > 0, i < 3, j > 0, j < 3] -- UDLRに行けるか
-- 移動した先
    udlr (i,j) = [(pred i,j), (succ i, j), (i, pred j), (i, succ j)]

-- 状態が空なら終了
    step ijbs | null ijbs = Nothing
-- 金庫に達した経路リストを出力、達していない状態リストは継続して計算
    step ijbs = Just (map (BSL.drop (fromIntegral $ length passcode) . snd) goaled, ijbs1)
      where
-- 金庫前とそうでないものに分けて
        (goaled, conts) = partition (((3,3) ==) . fst) ijbs
-- もう1ステップ進んだ状態
        ijbs1 =
          [ (ij1, BSL.snoc bs d)
          | (ij, bs) <- conts
-- MD5ハッシュによる鍵の確認
          , let passcheck = map (\c -> 'b' <= c && c <= 'f') $ take 4 $ show $ md5 bs
-- ドアがあり、鍵が開いている移動先と、経路の追加文字
          , (True, True, ij1, d) <- zip4 (doors ij) passcheck (udlr ij) "UDLR"
          ]
```

最初に見つかった経路そのものを返す。

```haskell
input = "hhhxzeay"

part1 = head . dropWhile null . findPaths
```

```
ghci> part1 "ihgpwlah"
["DDRRRD"]
ghci> part1 "kglvqrro"
["DDUDRLRRUDRD"]
ghci> part1 "ulqzkmiv"
["DRURDRUDDLLDLUURRDULRLDUUDDDRR"]
```

# パート2

`unfoldr` が停止するまで完全に計算し、最後にゴールする状態の経路を取り出す。

```haskell
part2 pc = [(p, BSL.length p) | p <- ps]
  where
    ps = last $ filter (not . null) $ findPaths pc
```

```
ghci> map snd $ part2 "ihgpwlah"
[370,370]
(1.32 secs, 2,996,958,424 bytes)
ghci> map snd $ part2 "kglvqrro"
[492]
(2.02 secs, 5,749,981,664 bytes)
ghci> map snd $ part2 "ulqzkmiv"
[830]
(2.44 secs, 7,558,820,008 bytes)
```
