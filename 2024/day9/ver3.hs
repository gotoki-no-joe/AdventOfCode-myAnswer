import Data.Char
import Data.List

import Data.Array

runner i f = readFile i >>= print . f . map digitToInt . init

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [Int] -> Int
part1 ds = sum $ map (uncurry (*)) $ consume ijs $ reverse ijs
  where
-- 位置と内容の対のリストにする
    ijs, ijs0 :: [(Int,Int)]
    ijs = zip [0 ..] $ concat $ zipWith replicate ds $ intersperse (-1) [0 ..]

    ijs0 = zip [0 ..] $ genFile 0 ds
    genFile  fid (d:ds) = replicate d fid  ++ genSpace (succ fid) ds
    genSpace fid (d:ds) = replicate d (-1) ++ genFile fid ds
    genSpace _ [] = []

-- 消費する
    consume ijijs@((i,j):ijs) pqpqs@((p,q):pqs)
      | i > p     = []                         -- 前と後ろが交差したら終わり
      | j /= -1   = (i,j) : consume ijs pqpqs  -- 前がファイルなら送り出す
                                               -- 前が空白のとき
      | q /= -1   = (i, q) : consume ijs pqs   -- 後ろにファイルがあれば前の空白に移動させて出力
                                               -- 後ろが空白なら
      | otherwise = consume ijijs pqs          -- 読み飛ばす

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: [Int] -> Int
part2 ds = sum $ zipWith checksum [0 ..] filesZ
  where
-- 要素数は必ず奇数。 div n 2 が空きブロックの個数、1 + div n 2 がファイルの個数
--    n = length ds
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
-- 空き領域データを修正：サイズsの先頭を削除、サイズ ssz - sz の空きブロックが位置 spos + sz に出現
--      newsize = ssz - sz
--      spaces1 = spaces // [(ssz, tail $ spaces ! ssz), (newsize, insert (spos + sz) (spaces ! newsize))]
--      spaces1 = modifyArray ssz tail $ modifyArray newsize (insert (spos + sz)) spaces
        spaces1 = accum (flip ($)) spaces [(ssz, tail), (ssz - sz, insert (spos + sz))]
-- ファイル番号、サイズ、位置からチェックサム
--    checksum i (s, p) = i * sum [p .. p + pred s]
    checksum i (s, p) = i * div (s * (p + p + pred s)) 2

-- これを使う方が意図がはっきりするが、immutable arrayなので遠慮…しなくていいな！いや、もっといい方法がある。
modifyArray i f arr = arr // [(i, f $ arr ! i)]
