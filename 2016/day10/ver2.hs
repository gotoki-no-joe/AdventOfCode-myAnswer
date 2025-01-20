-- 2025/1/15 for spoiler

{-
少しややこしい。
静的な情報として、各ロボは二つチップを得たときどうするか、という動作指令。
これは、ロボの番号から取り出せる辞書に入れておきたい。

動的な情報として、いろいろなチップがどこにあるか。
outputの何番に、どんなチップが届いたか。
ロボは手持ちがないか、一つ持っているか。
ロボが二つ持った後に排出したり、入力にあったりする、誰かに渡すキュー。

問題の答えとしては、ロボの動作の過程で、61と17を比較したロボの番号だけ知りたい。
最終結果ではなくて。なんだそれ、という感じだけど、
「誰が何をした」というイベントをログに残して、そこから見つけるのがいいのかな。

以前は、ロボは一度しか発動しない、と決めつけて、それを拾って答えにしていた。

番号は0から始まる。ロボもoutputも。符号でごまかせない。+1しない限り。
-}

{-
data Inst
  = Que Int Int -- Que a b : チップaをロボまたは箱bへ value 5 goes to bot 2
  | Bal Int Int -- bot 2 gives low to bot 1 and high to bot 0

これを作る必然性ないな。

入力を解析して、ロボ番号に対して give の情報を IntMap (Int,Int) で蓄える

再度入力を解析して、ロボに配るチップの情報を [(Int,Int)] で集めておく。
これをキューかスタックか何かとして、
ロボと箱に届いたチップの IntMap [Int] を状態として、
ロボに2つのチップが届いたとき、二手に分配する情報をキューに戻す、を計算する

ロボ0～は番号0～
箱0～は番号-1,-2,～
-}

import qualified Data.IntMap as IM

runner f = readFile "input.txt" >>= print . f . lines

part1 ls
  | any ((2 <) . length . snd) $ IM.assocs imZ = error $ show imZ -- 3以上入れられた誰かがいる
  | otherwise = (part1ans, part2ans)
  where
    roro "bot"    x = read x
    roro "output" x = - (succ $ read x)
    gives = IM.fromList
      [ (botid, (lowid, hiid))
      | 'b':l <- ls, let ws = words l
      , let botid = read $ ws !! 1 :: Int
      , let lowid = roro (ws !! 5)  (ws !! 6)  :: Int
      , let hiid  = roro (ws !! 10) (ws !! 11) :: Int ]
    chipins =
      [ (val, dest)
      | 'v':l <- ls, let ws = words l
      , let val = read $ ws !! 1 :: Int
      , let dest = roro (ws !! 4) (ws !! 5) :: Int]
    imZ = loop IM.empty chipins
    loop im [] = im
    loop im ((val, dest):chips) =
      case im1 IM.! dest of
        [v1, v2] | dest >= 0 -> loop im1 ((min v1 v2, lowid) : (max v1 v2, hiid) : chips)
        _ -> loop im1 chips
      where
        im1 = IM.insertWith (++) dest [val] im
        (lowid, hiid) = gives IM.! dest
    part1ans = [rid | (rid, chips) <- IM.assocs imZ, elem 61 chips, elem 17 chips]
    out012 = map (imZ IM.!) [-1, -2, -3]
    part2ans = (out012, product $ concat out012)
