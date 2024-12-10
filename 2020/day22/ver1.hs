{-
part1は解けているのにファイルが何も残っていない。
しかたないのでやり直す。

二人のカードはSequenceで保持するべきだ。
-}

import qualified Data.Sequence as Q
import Data.Foldable
import Data.List
import qualified Data.Set as S

import qualified Data.Vector.Unboxed as UV
import Data.Word
import qualified Data.ByteString as V

runner i f = do
  ls <- lines <$> readFile i
  let (_:ls1, _:_:ls2) = break null ls
  let ans = f (map read ls1) (map read ls2)
  print ans

test1 = runner "sample.txt" part1
main1 = runner "input.txt"  part1

part1 :: [Int] -> [Int] -> (Int, Int)
part1 as bs = (score aqZ, score bqZ)
  where
    aq0 = Q.fromList as
    bq0 = Q.fromList bs
    (aqZ,bqZ) = until ende fight (aq0,bq0)
    ende (aq,bq) = Q.null aq || Q.null bq
    fight (a Q.:<| aq, b Q.:<| bq)
      | a > b = (aq Q.:|> a Q.:|> b, bq)
      | True  = (aq, bq Q.:|> b Q.:|> a)
    n = length as + length bs
    score = sum . zipWith (*) [n, pred n ..] . toList

{-
ghci> test1
(0,306)
ghci> main1
(33925,0)

負けてないが？
-}

{-
実データは、カード50枚なので25枚ずつスタート。
番号も多分ぴったり1から50
-}

-- 番号チェック
checkNumber as bs = filter (uncurry (/=)) $ zip [1 ..] $ sort (as ++ bs)

{-
ghci> runner "sample.txt" checkNumber
[]
ghci> runner "input.txt" checkNumber
[]
おけ。

そして小さな数で、シーケンスを色々記憶していかないとマズそうなので、
メモ化ができたらと思うけど、static dp だと、必要な状態の洗い出しが大変。
50枚のカードの任意の並べ替えが 50! と、二人に分ける切り方がそれぞれ51通りなので、
結局51! = 1551118753287382280224243016469303211063259720016986112000000000000
状態を全て作る形でしか、Data.Mapによるstatic DPはできない。
計算で出てきた所だけを気にするには、メモ化でないと。
状態をなるべく小さいデータ構造で表すために、Vector of Word8 とか持ち出さないとダメかな。
これは、mutableな言語の方が得意そうだ…

そういえばこの書きぶりだと、primaryなゲームは、千日手で終わることはないみたいだね。
ゲームの勝敗はカードがなくなったとき、という言い方で締めてるから。
そんなことないかな？
-}

part2 as bs = (score avZ, score bvZ)
  where
    av0, bv0 :: UV.Vector Word8
    av0 = UV.fromList $ map fromIntegral as
    bv0 = UV.fromList $ map fromIntegral bs

    (avZ, bvZ) = game av0 bv0

-- サブゲームを含めて、ゲームを一つ完了させる
-- 既出の状態を管理して、千日手に備える
    game av bv = rounds S.empty av bv
    rounds s av bv
      | S.member (av,bv) s = (av, bv) -- 千日手でプレイヤー1の勝ち
      | UV.null av || UV.null bv = (av, bv) -- 片方が空になったらそうでない方の勝ち
      | gosub = if subwin  then win1 else win2
      | True  = if a0 > b0 then win1 else win2
      where
        a0 = fromIntegral $ UV.head av
        b0 = fromIntegral $ UV.head bv
        alen = UV.length av
        blen = UV.length bv
        gosub = a0 <= pred alen && b0 <= pred blen -- サブゲームに入る条件
        subwin = not $ UV.null $ fst $ game (UV.take a0 $ UV.tail av) (UV.take b0 $ UV.tail bv) -- サブゲームの結果
        s1 = S.insert (av,bv) s
        win1 = rounds s1 (UV.concat [UV.tail av, UV.take 1 av, UV.take 1 bv]) (UV.tail bv)
        win2 = rounds s1 (UV.tail av) (UV.concat [UV.tail bv, UV.take 1 bv, UV.take 1 av])

    n = length as + length bs
    score = sum . zipWith (*) [n, pred n ..] . map fromIntegral . UV.toList

test2 = runner "sample.txt" part2
main2 = runner "input.txt"  part2

{-
ghci> test2
(0,291)
ghci> main2
(33441,0)
特にメモ化とかしなくても、愚直なsimulationで計算できてしまった。拍子抜け。
-}
