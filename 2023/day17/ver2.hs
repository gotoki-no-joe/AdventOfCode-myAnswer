import Data.Array
import Data.Char

runner i f = do
  dss <- map (map digitToInt) . lines <$> readFile i
  let h = length dss
      w = length $ head dss
      fld = listArray ((1,1),(h,w)) $ concat dss
  print $ f fld

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 fld = 0
  where
    bnds = bounds fld

{-
現在位置と向きを状態に持ち、さらに熱損失の累計をキーとして優先度付きキューにエージェントを入れる
つまりダイクストラ法のようなことをする
グラフを全て作ってからするのは面倒なので全探索、と思ったけれど、
重複が消せないとそれはそれで破綻するか？
-}