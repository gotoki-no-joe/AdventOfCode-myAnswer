{-# LANGUAGE PartialTypeSignatures #-}

{-
何このジェンガ。

一つ一つはそれこそ、Ixの機能で全てのマス座標に展開できる。
ブロックはリストで持っておけばいい。

z座標の昇順にソートしないといけない様だ。
逆L字型とかないから、最初から干渉していない限り、zが等しいブロックが複数あっても、
どちらを先に動かしても問題ないはず。

1250個もブロックあるけどzは250程度しかないようだ。まぁ配列で扱わないから何でもいい。

で、既に位置が固定されたブロックのマスを集合に写しておく。
移動先の高さも反映させておくべきか。
次に低い位置にあるブロックを、邪魔がない限り1マスずつ下にズラしてみる。
置けなくなるギリギリの高さに固定して、集合に乗せる、を全部やる。

あとは、それぞれを抜いたときに、落ちるやつがいるか調べる。
…どうやって？
自分の上に乗っているブロックを特定する。そいつが誰かの上に乗っている、その誰かが自分しかいなかったらアウト。

これは、落ちる側からすれば、落着した瞬間に判別できることか。
着地したときに、着地先のブロックが一つだったら、そいつが抜けたら自分はアウトになるから。
それがわかるようにするためには、世界のブロック集合に色を塗っておく必要があるので、SetでなくMapでやる。


-}

import qualified Data.Map as M
import Data.Ix
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Set as S
import Data.Array

import Debug.Trace

runner i f = do
  bss <- map (map read . wordsBy (not . isDigit)) . lines <$> readFile i
  print $ f bss

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [[Int]] -> _
part1 bss = length bss - S.size disintegrates
  where
-- ブロック群を高さ順にソートして並べる
    blocks = sort $ map expand bss
-- 一つずつ落とす
    (spaceN, blocks1) = mapAccumL gravity M.empty $ zip [1 ..] $ map snd blocks
-- ひとつのブロックを見て、全てのcellについて、z-1なマスをspaceで調べる
-- そこがz=0、つまりz=1なものがあれば自分は落ちないのでヨシ。
-- そうでないとき、自分でも空白でもない他のブロックに乗っているとき、誰に乗っているかを集める。
-- それが一色のとき、そのブロックが消えると自分は落ちるということなので、
-- そいつは抜いたら危険だ（俺にとって）とマークする
-- を全てのブロックについて調べると、抜いたらマズいブロックが全部わかる。
    disintegrates = S.fromList
      [ head unders
      | (idnum, block) <- zip [1 ..] blocks1
      , all (\(_,_,z) -> z > 1) block
      , let unders = [j | (a,b,c) <- block, let j = M.findWithDefault idnum (a,b,pred c) spaceN, j /= idnum]
      , not $ null unders -- Z=0とだけ接している場合
      , all (head unders ==) unders
      ]

-- 6つの数によるブロックの定義を、座標に展開する
-- 最も低い座標位置も付ける
expand (a:b:c:d:e:f:_) = (min c f, range ((a,b,c),(d,e,f)))

-- 一つ落とす
gravity space (idnum, block) = (space1, block1)
  where
    block1 = last $ takeWhile noconflict $ iterate godown block
    noconflict block = all (flip M.notMember space) block && all (\(_,_,z) -> z > 0) block
    space1 = M.union space $ M.fromList [(cell, idnum) | cell <- block1]
    godown = map f where f (a,b,c) = (a,b,pred c)

{-
パート2

連鎖反応
パート1で調べた、「コイツを消すと俺は落ちる」は1-n関係なので、そのグラフを作る。
これは森になる。
それぞれから到達可能な頂点の個数を数えて足し合わせればよい。

違う、二つのブロックに乗っていて安心だったブロックも、両方消えたら落ちるから、
一つに限定せず、「自分はこれらの上に乗っている」というn-n関係のグラフで、
「自分の全ての依存先がマークされていたらマークする」のDPを、
ひとつのブロックだけマークした状態から伝播させる、をしないとならないのか。めんどいな。
-}

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: [[Int]] -> _
part2 bss = subtract n $ sum $ map countFalls [1 .. n]
  where
    n = length bss
-- ブロック群を高さ順にソートして並べる
    blocks = sort $ map expand bss
-- 一つずつ落とす
    (spaceN, blocks1) = mapAccumL gravity M.empty $ zip [1 ..] $ map snd blocks
-- ひとつのブロックを見て、全てのcellについて、z-1なマスをspaceで調べる
-- 地面をid=0とする
-- そうでないとき、自分でも空白でもない他のブロックに乗っているとき、誰に乗っているかを集める。
-- それらが全て落ちると自分は落ちる、という依存グラフを構築する。
    depends = listArray (0,n) $ [] :
      [ if any (\(_,_,z) -> z == 1) block then [0] else nub unders
      | (idnum, block) <- zip [1 ..] blocks1
      , let unders = [j | (a,b,c) <- block, let j = M.findWithDefault (if c == 1 then 0 else idnum) (a,b,pred c) spaceN, j /= idnum]
      ]
-- 指定したブロックを抜いて、連鎖的に落ちるブロックの個数を求める
-- 地面ID=0はFalse、指定ブロックはTrue、あとはdepends全てのandをとる。
-- この配列のTrueの個数-1が答えなので、全部足して最後にN引こう。
    countFalls k = length $ filter id $ elems arr
      where
        arr = arrayDP (0, n) nf
        nf 0 = ([], const False)
        nf i | i == k = ([], const True)
            | otherwise = (depends ! i, and . map snd)

-- array bnds nf は、添え字範囲bndsの集めるDPを行う
-- nf i は (js, fi) を返す
-- js は添え字 i の値を求めるためにDP値を集める先のリスト
-- fi は js の要素について集めてきたDP値の対のリストから、iの値を求める関数
-- 結果は、bns範囲の配列で、DPの答えが入っている
arrayDP :: Ix i => (i, i) -> (i -> ([i], [(i, b)] -> b)) -> Array i b
arrayDP bnds nf = dpArr
  where
    dpArr = listArray bnds [fi [(j, dpArr ! j) | j <- js] | i <- range bnds, let (js, fi) = nf i]
