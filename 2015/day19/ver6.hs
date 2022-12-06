-- 2022-11-24
import Data.Char
import qualified Data.IntMap as IM
import qualified Data.Set as S
import Data.List

exec1 fn = do
  ls <- lines <$> readFile fn
  let lN = encode $ last ls
  let rules = map parse $ init $ init ls
  print $ part1 rules lN

parse :: String -> (Int, [Int])
parse xs = (head $ encode w1, encode w3)
  where
    (w1:_arrow:w3:_) = words xs

encode :: String -> [Int]
encode (c1:c2:cs)
  | isLower c2 = ord c1 * 256 + ord c2 : encode cs
encode (c:cs)  = ord c : encode cs
encode "" = []

part1 rules molec0 = S.size s
  where
    rs = IM.fromListWith (++) [(l,[r]) | (l,r) <- rules]
    (_, s) = foldr step ([], S.empty) molec0

    step elem (elems, s) = (elem:elems, S.union s1 $ S.map (elem :) s)
      where
        s1 = S.fromList [es ++ elems | es <- IM.findWithDefault [] elem rs]

electron = encode "e"

-- FAIL
part2 rules molec0 = loop 0 S.empty S.empty [molec0]
  where
    loop cnt visited yS []
      | S.member electron visited = cnt
      | S.null yS = error "empty next"
      | otherwise = loop (succ cnt) (S.union visited yS) S.empty (S.elems yS)
    loop cnt visited yS (molec:molecs) = loop cnt visited (S.union yS (ms S.\\ visited)) molecs
      where
        (_, ms) = foldr step ([], S.empty) molec
        step elem (elems, s) = (elems1, S.union s1 (S.map (elem :) s))
          where
            elems1 = elem:elems
            s1 = S.fromList
              [ l : drop (length r) elems1
              | (l,r) <- rules, isPrefixOf r elems1
              ]

exec2 fn = do
  ls <- lines <$> readFile fn
  let lN = encode $ last ls
  let rules = map parse $ init $ init ls
  part2b rules lN

-- 可能な限りの書き換えをドンドン施してしまう、というやり方でスタートまで行けるかやってみる。

-- FAIL
part2a rules molec0 = loop molec0
  where
    rulesM = IM.fromListWith (++) [(head r, [lr]) | lr@(_,r) <- rules]
    loop molec
      | cnt == 0 = print ("end", molec) >> return ()
      | otherwise = print (cnt, molec1) >> loop molec1
      where
        (molec1, cnt) = foldr step ([], 0) molec
        step m (ms, cnt)
          | null reps = (m:ms, cnt)
          | singleton reps = (head reps, succ cnt)
          | otherwise = error (show reps)
          where
            ms1 = m : ms
            reps =
              [ l : drop (length r) ms1
              | (l,r) <- IM.findWithDefault [] m rulesM
              , isPrefixOf r ms1
              ]

singleton [_] = True
singleton _ = False

-- 右辺の先頭文字でグループ分けして、残りでprefix確認して、適用して、そのまま続ける。
-- 複数適合する場合は落ちるようにするか。

{-
> exec2 "input.txt"
(164,[72,17249,80,21102,70,16754,17249,17249,17249,21353,21102,21353,21102,66,70,16754,17249,70,89,70,16754,17249,80,21102,70,16754,21353,21102,17249,70,89,17249,17249,17249,17249,70,16754,17249,80,66,17249,17249,17249,17249,17249,21353,21102,66,17249,21353,21102,17249,21102,70,16754,89,80,19815,16754,70,16754,80,21102,70,16754,80,66,70])
(29,[72,21353,21102,17249,70,89,70,16754,17249,17249,17249,21353,21102,66,21353,21102,17249,21102,70,16754,89,70,16754,70,16754,17249,70])
(7,[72,21353,21102,66,21353,21102,17249,21102,70,16754,89,70,16754,70,16754,70])
(1,[78,21102,66,21353,21102,17249,21102,70,16754,89,70,16754,70,16754,70])
("end",[78,21102,66,21353,21102,17249,21102,70,16754,89,70,16754,70,16754,70])
201回の書き換えで停滞してしまった。
-}

{-
マッチを探す向きを後ろからに固定して、ひとつマッチしたらまた末尾からやり直し、というスタイルにしてみよう。
-}

part2b rules molec0 = loop 0 molec0
  where
    rulesM = IM.fromListWith (++) [(head r, [lr]) | lr@(_,r) <- rules]
    loop cnt molec
      | acted = print (length molec1) >> loop (succ cnt) molec1
      | otherwise  = print ("end", molec, cnt) >> return ()
      where
        (molec1, acted) = foldr step ([], False) molec
        step m (ms, True) = (m:ms, True)
        step m (ms, False)
          | null reps = (m:ms, False)
          | singleton reps = (head reps, True)
          | otherwise = error (show reps)
          where
            ms1 = m : ms
            reps =
              [ l : drop (length r) ms1
              | (l,r) <- IM.findWithDefault [] m rulesM
              , isPrefixOf r ms1
              ]

-- 正しく [101] になって停止した。Yeah!
