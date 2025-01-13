もし列が長大なら、先頭2要素を見て列が上昇列か下降列かを決めてから（あるいは下降列になっていたら符号反転する手もある）、一度だけ使える数の読み飛ばしをしたかどうかで状態を持って検査をする、簡単なDPをする必要があるだろう。

```haskell
prop2 :: [Int] -> Bool
-- 長さ2以上なら、上昇列に固定して
prop2 xs@(x1:x2:_) =
  case compare x1 x2 of
    LT -> sub True xs
    EQ -> False
    GT -> sub True $ map negate xs
  where
    -- 一度だけ条件に合わないx2を無視できる
    sub _ [_] = True
    sub ok (x1:x2:xs)
      | x1 < x2, x2 - x1 <= 3 = sub ok (x2:xs)
      | otherwise             = ok && sub False (x1:xs)
-- 長さ1以下なら常に問題なし
prop2 _ = True

part2 :: [[Int]] -> Int
part2 = length . filter prop2

test2 = runner "sample.txt" part2
main2 = runner "input.txt"  part2
```

と思ったけど、8 6 4 4 1 の 右の4を消しても手遅れで、左の4を消してやり直す必要がある。
なので、「エラーになる箇所」が1箇所のときその前後、
連続する2箇所のとき中央の値、
とか、面倒くさいことになる。

