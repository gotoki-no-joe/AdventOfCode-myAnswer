# 入力

1文字ごとの数に直したリストを渡そう。
本番データは20,000文字ある。全て9の最悪の場合で180,000ブロック。
直接扱ってもまだ対応できる規模。

```haskell
import Data.Char

runner i f = readFile i >>= print . f . map digitToInt

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [Int] -> Int
part1 ds = ...
```

# パート1

1ブロックごとに空きをギチギチに詰めるし、とりあかずナイーブな方法で計算する。
ブロックの位置と、ファイル番号(0～)または空き(-1)という情報のペアのリストに展開する。

```haskell
-- 位置と内容の対のリストにする
    ijs, ijs0 :: [(Int,Int)]
    ijs = zip [0 ..] $ concat $ zipWith replicate ds $ intersperse (-1) [0 ..]
-- 実際こんなコードをいきなり書けなくて、下の垢抜けないコードで最初は突破した
    ijs0 = zip [0 ..] $ genFile 0 ds
    genFile  fid (d:ds) = replicate d fid  ++ genSpace (succ fid) ds
    genSpace fid (d:ds) = replicate d (-1) ++ genFile fid ds
    genSpace _ [] = []
```

このリストを前と後ろからの両方から読み、

- 後ろの空白は読み飛ばす、ファイルは前方の空白に移動させる
- 前のファイルは出力する、空白は後方のファイルを持ってくる
- 前と後ろの位置関係が逆転したら処理完了

とやれば、移動後の各ファイルの位置が得られる。

```haskell
-- 消費する
    consume ijijs@((i,j):ijs) pqpqs@((p,q):pqs)
      | i > p     = []                         -- 前と後ろが交差したら終わり
      | j /= -1   = (i,j) : consume ijs pqpqs  -- 前がファイルなら送り出す
                                               -- 前が空白のとき
      | q /= -1   = (i,q) : consume ijs pqs    -- 後ろにファイルがあれば前の空白に移動させて出力
                                               -- 後ろが空白なら
      | otherwise = consume ijijs pqs          -- 読み飛ばす
```

ファイルの位置だけが出てくるので、掛け合わせて足せば答えになる。

```haskell
part1 :: [Int] -> Int
part1 ds = sum $ map (uncurry (*)) $ consume ijs $ reverse ijs
  where
    ...
```

`consume` の時点で掛け算してしまえば早かったが、SRPということで。

# パート2

後ろのファイルが入りきるような、最も手前の空白に移動させる、を、後ろのファイルから順に全てについて行う。
パート1のような持ち方だと、長さを毎回数えることになって無駄すぎる。

その要素の長さと、ファイル番号(0～)または空き(-1)の対のリストを管理し、
また、別でファイルそれぞれの大きさを調べておき、後ろのファイルから順に

- リストを前から調べ、自分の収まる空きを探し
  - それを見つける前に自分自身と遭遇したときは移動なし、元のリストのままにする
  - 空白が見つかったならば、そこに自分を収め、まだ空きが残るならお釣りの分の空きを新たに作り、
さらに後ろに移動前の自分が出てくるのでそれを空きに変える

（ファイルを移動させた際、後ろ側にできた空きの前後に空きがあっても、
これを連結するような正規化処理は不要。処理は後ろのファイルから順に行われ、
自分より後ろに移動することはないから。）

このリストは長さ $N = 2 \times 10^4$ で、リストを $O(N)$ 回舐めるので、 $O(N^2) = 4 \times 10^8$ の計算量になる。
実行時間的にかなりギリギリ寄りだ。
実際これでやると、インタプリタでは待てない。コンパイルして実行すると割とすぐに終わるけれど、
それで終わらせるのはちょっと残念。

動作の様子を少し想像してみると、リストの中にファイル要素が入っている必要は一切なく、

- 空きブロック列の開始位置と大きさ
- 各ファイルの（番号と）開始位置と大きさ

を別に持っておき、ファイルを後者から順に調べて、より前に移動できる空きブロックがあればそこに移動したように修正する、
を繰り返せばよい。そしてファイルの方の情報だけからチェックサムは算出できる。
これで、リストで表現したままでもリスト長が半分になり、その分軽くなる。

さらに、リストを前から舐める計算をなくせないか考えてみる。
移動させようとしているファイルより前にある空きブロックは元からあるもの（か、それが縮んだもの）だけで、
つまり大きさはたかだか1～9の範囲しかない。
そして知りたいことは、今移動させようとしているファイルサイズ以上のものの中で、最も手前にあるものの位置と大きさである。
空きブロックを、その大きさごとに、位置の順にリストにした配列で管理しておき、
移動させるファイルの大きさ以上の項目について、リストの先頭要素の最小値を見るだけでその答えは得られる。

移動を行ったらなら、その空きブロックが消費され、少し後ろにより短い空きブロックが出現することを
このデータ構造に反映させれば、線形探索がなくなる分ずっと軽い計算になるだろう。

ファイル番号 $i$ が位置 $p$ からサイズ $s$ であるとき、その分のチェックサムは
$i \times \left (p + (p+1) + \dots + (p+s-1) \right)$
で、`i * sum [p .. p + pred s]` としても `i * div (s * (p + p + pred s)) 2` としてもいい。

結局こんな感じになる。

```haskell
test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: [Int] -> Int
part2 ds = sum $ zipWith checksum [0 ..] filesZ
  where
-- 各要素の開始位置は長さの累積和で求められる
    ps = scanl (+) 0 ds
-- 空きブロックをそのサイズごとに分別して位置をリストに集め、昇順にしておく
    spaces0 = fmap sort $ accumArray (flip (:)) [] (0, 9) $
              [sp | (sp, True) <- zip (zip ds ps) $ cycle [False, True]]
-- ファイルのサイズと開始位置のリスト
    files0 = [sp | (sp, True) <- zip (zip ds ps) $ cycle [True, False]]
-- ファイルのリストを後ろから順に再配置
    (_spacesZ, filesZ) = mapAccumR step spaces0 files0
-- サイズsz,位置posのファイルをspacesのどれかの位置に動かすステップ動作
    step spaces f@(sz, pos)
      | null cands = (spaces, f) -- 引っ越し先なし
      | otherwise  = (spaces1, (sz,spos)) -- 移動実行
      where
-- 再配置候補の位置とサイズのリスト
        cands = [(head ps, s) | s <- [sz .. 9], let ps = spaces ! s, not $ null ps, let p = head ps, p < pos]
-- 最も手前にある再配置候補の位置とサイズ
        (spos, ssz) = minimum cands
-- 空き領域データを修正：サイズsszの先頭を削除、サイズ ssz - sz の空きブロックが位置 spos + sz に出現
        spaces1 = accum (flip ($)) spaces [(ssz, tail), (ssz - sz, insert (spos + sz))]
-- ファイル番号、サイズ、位置からチェックサム
--    checksum i (s, p) = i * sum [p .. p + pred s]
    checksum i (s, p) = i * div (s * (p + p + pred s)) 2
```
