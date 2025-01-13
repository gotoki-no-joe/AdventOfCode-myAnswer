{-# LANGUAGE Strict #-}

import Data.Char
import Data.List.Split
import qualified Data.Set as S
import Data.Word

import Debug.Trace

runner i f = do
  xss <- map parse . lines <$> readFile i
  print $ f xss

parse :: String -> [Int]
parse = map read . wordsBy (not . isDigit)

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

main = main1

type State = S.Set (Word8, [Word8], [Word16])

add xs ys = zipWith (+) xs $ map fromIntegral ys

part1 xss = (res, sum $ zipWith (*) [1 ..] res)
  where
    res = map compute xss
    initial :: State
    initial = S.singleton (24,[1,0,0,0],[0,0,0,0]) -- 残り日数、4種のロボ、4つの資源
    compute conf = maximum $ 0 : bigloop initial
      where
        _:c1:c2:c3:c4:c5:c6:_ = map fromIntegral conf
        bigloop sts
          | null sts = []
          | otherwise = traceShow (S.size nexts) $ res : bigloop nexts
          where
            (res, nexts) = loop sts S.empty

        loop sts nexts | S.null sts = (0, nexts) -- いらんはずなんだが…
        loop sts nexts = loopsub (S.deleteFindMax sts) nexts
        loopsub ((0,_,_), sts) nexts = (maximum $ (0 :) $ map (\(_,_,[_,_,_,geo]) -> geo) $ S.elems sts, nexts)
        loopsub ((d,robos,res@[ore,clay,obs,_]), sts) nexts = loop sts1 nexts1
          where
            d1 = pred d
            res1 = add res robos
            sts1 = S.insert (d1, robos, res1) sts
            nexts1 = (if ore >= c1               then S.insert (d1, add [1,0,0,0] robos, add [-c1,0,0,0]   res1) else id) $
                     (if ore >= c2               then S.insert (d1, add [0,1,0,0] robos, add [-c2,0,0,0]   res1) else id) $
                     (if ore >= c3 && clay >= c4 then S.insert (d1, add [0,0,1,0] robos, add [-c3,-c4,0,0] res1) else id) $
                     (if ore >= c5 && obs  >= c6 then S.insert (d1, add [0,0,0,1] robos, add [-c5,0,-c6,0] res1) else id) $
                     nexts

{-
ロボの台数合計ごとに計算を分ける。
状態のSetから、最大のものを取り出しては、遷移した状態を作る。
ロボ構成と資源の量により、常に、ロボを作らず、資源の確保だけする遷移がある。
これをSetに戻す。
また、ファクトリーを使ってロボを作りつつ、資源確保もする遷移がある。
これはロボの数を変えるので、後回しにするべく、違う集合に貯めるだけ貯めておく。
残り日数が0のしかなくなったら、geoの最大値を探して、
それと、次の周回のための集合を結果として返す。
という計算を、それ以上ロボが作れなくなるまで繰り返す。

sampleの1行を、メモリを飛ばさずに計算できるようにはなった。
しかしコンパイル実行でも我慢ならない時間がかかる。
trace見ててもつらい。

([9,12],33)
うん、あってる。
とりあえずWord導入して、メモリを削減してみる。
キャッシュ効率はある程度性能に寄与するはずだ。

13483693
18546999
ver2.exe: osCommitMemory: VirtualAlloc MEM_COMMIT failed to commit 1048576 bytes of memory  (error code: 1455):
ページング ファイルが小さすぎるため、この操作を完了できません。

コケた。おりょ。
sampleのほうで
17773938
17432595
までは耐えたんだがなぁ。

つまりやっぱり枝刈りないとつらい。（というか、このアプローチ合ってるのかなぁ？）
そのためには、ロボの構成ごとに整理して、
大きい方から処理していくときに、より小さいものを捨てていけばいい？
そいつが作る構成は、より小さい、には絶対にならない、ロボを足す分あるから。
これを無精して仕分けせずにするのはアホだが、コードめんどくせぇな。

ロボ構成をキーにしたMapで、中身は 残り日数と資源のリストのペアの集合、が大状態
とすると、走るのがやりにくい？
Map.assocs を引数に今の計算が走り、全て消費する。geoの結果と、次にばらまく情報はそれぞれでMapにしておいて、
UnionWith S.union で合体させればいいのか。
「今の計算が走り」を始める前に、大きい方から順に眺めて枝を刈る。
このタイミングだと、メモリに枝刈り前のSetが放置される。でもまぁ何とかなるのか。
もしやさらに、ロボのセットごとの計算は独立しているから並列化できる？
それは希望が出るなぁ？

並列化はメモリの奪い合いになるだけか。
それでも耐える巨大なメモリが載った並列計算機を使って力尽くで計算してみたいが。
どうせ今混雑しているはGPUマシンだけだろうし。
-}
