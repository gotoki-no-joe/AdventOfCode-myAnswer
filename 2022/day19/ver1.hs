{-
晶洞 (geode) 破壊ロボを作る。黒曜石が原料。そのために、
池の底から黒曜石 (obsidian) を採取するために、防水型黒曜石採取ロボを作る。
それには粘土 (clay) が必要。そのために、粘土集め専用ロボを作る。
それには鉱石 (ore) が必要。そのために、巨大ドリル装備鉱石集めロボを作る。
1台だけ手持ちがある。
ロボは1分ごとに1資源を回収する。資源を消費して、1分でロボを1つ作れる。
設計図があるが、ひとつしか選べない。
24分後に、geodeを回収できる最大個数で設計図を評価せよ。

どのロボに何が必要なのかの材料は決まっていて、量が違うだけなのぬ。
ore, clay, obsidian, geode の手持ちの量
ore-robo, clay-robo, obs-robo, geode-robo の台数
8つの整数の組が状態。
ロボの台数だけ、資源が増える。これは勝手に起きる。
各ロボの必要資源が溜まっているとき、制作を指示できる。これが選択可能な行動。
状態の重複はなく、メモ化も何もなく全探索するしかないように見えるんだが。
-}

import Data.Char
import Data.List.Split

import Data.IORef
import qualified Data.Map as M
import System.IO.Unsafe

import Debug.Trace

runner i f = do
  xss <- map parse . lines <$> readFile i
  print $ f xss

parse :: String -> [Int]
parse = map read . wordsBy (not . isDigit)

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

main = test1 >> main1

add :: [Int] -> [Int] -> [Int]
add = zipWith (+)

part1 xss = (res, ans)
  where
    initial = ([0,0,0,0],[1,0,0,0]) -- 4つの資源、4つの対応するロボ
    compute0 (idx:c1:c2:c3:c4:c5:c6:_) = maximum $ map ((!! 3) . snd) state24
      where
        state24 = iterate (concatMap step) [initial] !! 24
        step (res@[ore,clay,obs,_], robos) = robo1 $ robo2 $ robo3 $ robo4 [(res1, robos)]
          where
            res1 = add res robos
            robo1 = if ore >= c1               then ((add [-c1,0,0,0]   res1, add [1,0,0,0] robos) :) else id
            robo2 = if ore >= c2               then ((add [-c2,0,0,0]   res1, add [0,1,0,0] robos) :) else id
            robo3 = if ore >= c3 && clay >= c4 then ((add [-c3,-c4,0,0] res1, add [0,0,1,0] robos) :) else id
            robo4 = if ore >= c5 && obs  >= c6 then ((add [-c5,0,-c6,0] res1, add [0,0,0,1] robos) :) else id
-- state = (res, robos) として
-- 1分経つと資源を獲得するので res1 = zipWith (+) res robos とする前に、ロボを作る話が先。まぁ同時にできるけど。
-- res !! 0 >= c1 ならば zipWith (+) [-c1,0,0,0], zipWith (+) [1,0,0,0] した版が作れる
-- res !! 0 >= c2 ならば zipWith (+) [-c2,0,0,0], zipWith (+) [0,1,0,0] した版が作れる
-- res !! 0 >= c3, res !! 1 >= c4 ならば [-c3,-c4,0,0], [0,0,1,0]
-- res !! 0 >= c5, res !! 2 >= c6 ならば [-c5,0,-c6,0], [0,0,0,1]
-- 作れないか、作れるけど作らないかで、分岐しないこともできる。

{-
ウンともスンともいわんな。
残りタイムと状態を引数に、最終的なgeodeの個数だけ返すDFSな再帰関数を、
メモ化してみようか。
-}
    compute [idx,c1,c2,c3,c4,c5,c6] = memoize recur (24, [0,0,0,0], [1,0,0,0])
      where
        recur _f (0, [_,_,_,geo], _) = geo
        recur rf (t, res@[ore,clay,obs,_], robos) = maximum $
            [rf (t1, add [-c1,0,0,0  ] res1, add [1,0,0,0] robos) | ore >= c1              ] ++
            [rf (t1, add [-c2,0,0,0  ] res1, add [0,1,0,0] robos) | ore >= c2              ] ++
            [rf (t1, add [-c3,-c4,0,0] res1, add [0,0,1,0] robos) | ore >= c3 && clay >= c4] ++
            [rf (t1, add [-c5,0,-c6,0] res1, add [0,0,0,1] robos) | ore >= c5 && obs  >= c6] ++
            [rf (t1,                   res1,               robos)]
          where
            t1 = pred t
            res1 = add res robos

    res = map compute xss
    ans = sum $ zipWith (*) [1 ..] res

memoize :: Ord d => ((d -> a) -> (d -> a)) -> (d -> a)
memoize mf = f
  where
    memo = unsafePerformIO $ newIORef M.empty
    f x = unsafePerformIO $ do
      m <- readIORef memo
      case M.lookup x m of
        Just a -> return a -- putChar '!' >> return a -- ヒットしたら知らせる…ってめちゃくちゃヒットしてた。しかし終わらない。
        Nothing -> do
          let a = mf f x
          modifyIORef' memo (M.insert x a)
          return a

{-
> ./ver1
ver1.exe: osCommitMemory: VirtualAlloc MEM_COMMIT failed to commit 1048576 bytes of memory  (error code: 1455): ページング ファイルが小さすぎ
るため、この操作を完了できません。

ばんばかヒットするメモが溢れて終わったようだ…
-}

{-
どうすりゃいいのよ。

obsidianロボが完成した瞬間から逆算する？

ロボの手持ち台数で世代を分ける。ロボは増えたら減ることはないし、台数が同じ状態の間で遷移することはないから。
そして、手持ち台数のconfigurationの中で、出現しうる状態全てから始めて、遷移する先を最終時刻まで、
別のconfigurationに遷移するものも全て生成する。その設定の中で完了するものについてはgeoの最大値も求める。
configurationの中では、手持ちの石の個数と残り日数を考える。
日数が若いのに全ての面について自分を凌駕している状態がある劣った状態は枝刈りしていいが、
それを特定するのはそれはそれで時間がかかる。同じ設定を二度計算しなければそれでヨシとするか？

一番安いロボがore=1で作れるとき、掘り続けて作り続けて24分間で作れる最大値がロボ数の上限、にできるが、
上限決めてidxで扱うか、気にせずInt Tupleで扱うか、どうしたらいいかね。面倒がない方で。

それより、あるconfigurtionについて計算しているときに、次の世代へのinsertが起きまくるんだが、
それはそもそもconfigurationごとに区別することを諦めて、ロボ台数による区別だけにして計算するべきか？
記憶容量をなるべく減らしたいが。
-}
