import Data.Char
import Data.List.Split
import Data.Array
import Data.Bits

import Data.List

import Debug.Trace
import Data.Maybe

-- import qualified Data.IntMap as IM
import qualified Data.Map as M

{-
先頭のVを除くと、大文字が位置の名前、数字文字でレート、それだけ抜き出す。
-}

runner i f = do
  ls <- map parse . lines <$> readFile i
  print $ f ls

parse :: String -> (String, Int, [String])
parse l = (vv, rate, uus)
  where
    vv = take 2 $ drop 6 l
    rate = read $ takeWhile isDigit $ drop 23 l
    uus = wordsBy (not . isUpper) $ drop 49 l

test1 = runner "test.txt" part12
main1 = runner "input.txt" part12

main = test1 >> main1

type State = M.Map (Int, Int) Int

part12 ls = (ans1, ans2)
  where
-- 並び順で背番号を付ける
    name2id = M.fromList [(vv, i) | ((vv,_,_),i) <- zip ls [1 ..]]
    n = length ls
-- 有向グラフとrateの配列
    rate :: Array Int Int
    rate = listArray (1,n) [r | (_,r,_) <- ls]
    g :: Array Int [Int]
    g = listArray (1,n) [map (name2id M.!) uus | (_,_,uus) <- ls]

{-
現在位置の頂点と、開放した、またはスルーした頂点の集合をキーに、
その構成で獲得できる最大スコアを値として保持するDPを行う。

リストで保持し、集合の包含関係に対応するスコアの単調性を用いて、不要な要素を除去する。
現在位置をキーにした配列に保持する、にしよう。かなり変わった。
-}
-- 初期状態は "AA" にいて、バルブは全て閉じで、スコア0
    state0 :: State
    state0 = M.singleton (name2id M.! "AA", 0)  0
-- 30ステップ遷移させる
    state30 = foldr step state0 [1 .. 29]
-- それぞれの状態について
-- 未解放のとき、移動せずバルブ開放できる。このときスコアを、rate*残り時刻だけ増やす。
-- 開放のいかんに拘わらず、隣接頂点に移動できる。
    step :: Int -> State -> State
    step time state = M.fromListWith max $ concatMap (stepFunc time) $ M.assocs state
    stepFunc :: Int -> ((Int,Int),Int) -> [((Int,Int),Int)]
    stepFunc time ((i, js), score)
      | openable  = valveOpen : moves
      | otherwise = moves
      where
        js1 = setBit js i
        moves = [((j, js), score) | j <- g ! i]
        openable = rate ! i > 0 && not (testBit js i)
        valveOpen = ((i, js1), score + time * rate ! i)
-- 結果
    ans1 = maximum $ M.elems state30

-- パート2
    state26 = foldr step state0 [1 .. 25]
-- そのまま総当たりするにはちょっと数が多い
    s26ents =
      foldl regstep [] $ -- 要らないものは除いて
      sortBy (\(_,s1) (_,s2) -> compare s2 s1) $ -- スコアの降順に整列して
      map (\((_,js),s) -> (js, s)) $ M.assocs state26 -- 最終位置を捨てて
    regstep es e@(js,s)
      | cond = es
      | otherwise = e:es
      where
        cond = any (\(jt,_) -> js .&. jt == jt) es -- （スコアが以上でかつ）訪問集合が部分集合なものがある
{-
part12 ls = (M.size state26, length s26ents) -- ans1 -- (ans1, ans2)
ghci> test1
(320,64)
ghci> main1
(90706,1772)
だいぶスリムになったので、これで総当たりする
-}
    ans2 = maximum
      [ s1 + s2
      | (js1,s1):sts <- tails s26ents
      , (js2,s2) <- sts
      , js1 .&. js2 == 0
      ]

{-
ghci> test1
(1651,1707)
ghci> main1
(2253,2838)

パート1より速い。

ver2から外したのはIntSetだけで、やらない方がマシ、という結末だった。
rate=0バルブのことを忘れると痛い目を見るぞ、という教訓は得られた。
-}


-- あかん、毎回正規化するのはやはり重すぎる。返ってこなくなった。
-- そうか、そういうもんか。コンパイルしてすら重い。
-- 余計なものと比較する時間は無駄すぎるので、ノード番号ごとに分割してやるべきか。

-- したら、コンパイルすれば我慢の限界を超える前に答えが出る、というレベル。
-- パート1での、少なくとも毎回の正規化は正しくないんだな。
-- そうなら、今の Array Int(ノード番号) [(Int(ノード集合),Int(スコア))] を
-- Array Int(ノード番号) (IntMap(ノード集合) Int(スコア))
-- にするのと、ver2の巨大なマップ Map (Int(ノード番号), Int(ノード集合)) Int(スコア)
-- と、どちらが正しい設計なんだろう？
-- Arrayに乗りかけたので、効率重視で突き進んでみようか。

-- ひとつのでかいMapなver2より圧倒的に遅いままでした。
-- 何かしくじったか、accumArray + IM.fromListWith max * N 毎回やるのがアホだったか。
-- IntSetを使わない版にすることにどれほどの意味があるかという感じだが、どう戻す？

-- rate = 0 のを立てておかなかったから開けてた、が関係している？
-- 思いっきりそれでした。そんなに影響あるの？無意味な同点状態が爆発的に増えるのか。
