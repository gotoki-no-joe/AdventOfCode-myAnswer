import Data.Bits
import Data.List
import Data.Word
import qualified Data.Set as S
import Debug.Trace

{-
すべての状況は、5つのものが4つのフロアのいずれかにあるから4^5 = 2^10 = 1024
その多くは、MがGを抑えずに他のMを焼損させる禁止状態
現在のフロアにあるものから1つまたは2つを必ず持って移動する。(0ではEが動かないから。）
というのが状態間のリンク。
input.txtの初期状態から全て4にある目標までの最短経路を求める。

普通に幅優先探索で、
探索済みの状態集合と、禁止状態の集合と、
今後探索するべき状態の集合を持ち回す感じかな。

おっと、本番はもっと要素が多くなるから状態も大きくなるか。
31個なら64bitで収まるんだけど。

状態表現に[Int]とかやると大きすぎる。それぞれは2ビットで済むのだから。
とはいえビットに詰め込むと処理が面倒。
'1'-'4'のbytestringで扱うのが手頃かしら。

目標状態から始める方が楽かもしれない。一緒かなぁ。
それとも、ちゃんとしたアルゴリズムを持ってくるべき？

ダイクストラ法を持ち出さなくても、この設定なら幅優先探索で充分だってさ。

それよりも、完全なグラフをいきなり作れないので、Data.Graphが多分使えない。
走りながら作っていくしかない。

-}

{- input.txt
The first floor contains a promethium generator and a promethium-compatible microchip.
The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator.
The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip.
The fourth floor contains nothing relevant.

4 :
3 :            CoM     CuM     RM    PlM
2 :                CoG     CuG    RG     PlG
1 : Ev PrM PrG

5対。10x2の20ビット。または11文字のbytestring
結構メモリくいそうだからIntで表現するか。

それぞれの要素が1～4階にいることを0～3で表す。
Evが状態遷移を起こす起点として重要だから、最下位に置く。後はそのまま順。

いや面倒だから、(Int,[Int],[Int])がダメだったらにしよう。
-}

{-
何が禁止状態か、がややこしい。

（自分(Ev)は防護服を着ているから被爆しない。無関係。）
generatorは常に放射している。危険。同じフロアにあるmicrochipは基本的に焼損する。
ただし、そこに対応するgeneratorがあるときは遮蔽されて、他のgeneratorがあってもなくても安全。

つまり、同じフロアに対応するgeneratorがなく、よそのgeneratorがあるmicrochipは焼損する。

(e,ms,gs)と呼ぶことにすると、
zip ms gs |> filter (uncurry (/=)) |> map fst |> map (flip elem gs) |> not.null
のとき禁止。

あと、eのあるフロアに何もない状態も禁止。移動できないから生成されないけれど。

この方向の表現は計算が実は面倒かもしれない。

それぞれのフロアに何番のものがあるか、0～9の要素の集合で表すこともできる。Evは別。

(Int,Array (1,4) (Set [1..5]), Array (1,4) (Set [1..5]))

これの値を(e,mas,gas)と呼ぶことにすると、
for i in {1..4} . exists (mas !! i \\ gas !! i) && exists gas !! i
なものがあるとダメ。
集合はたかだか5ビットのビット配列で表現する。
4フロアは長さ4のリストで表現しよう。

すると、
not $ null [ () | (ma,ga) <- zip mas gas, ga /= 0, ma .&. complement ga /= 0]
が禁止の条件。

ゴールは
(3,[0,0,0,31],[0,0,0,31])

開始は
(0,[1,0,14,0],[1,14,0,0])

次に、1つか2つ選んで隣のフロアに移動させるのはどうする。
ひとつの数に収まっていてくれないと面倒だ。
でも前者だともっと面倒だからこれでいいか。
たかだか10C1+10C2だから、全部作っておいて「やれるものだけ」やればいいのか。
1,2,4,8,16の組み合わせ。

うーん面倒だ。難しいのではなくて面倒だ。
-}

choices :: [Word16]
choices = [ b .|. c | (b:bs) <- tails (0:singles), c <- bs ] where
  singles = [ bit k | k <- [0..4] ++ [8+0..8+4] ]

type State = (Word16, [Word16])

isSafe :: State -> Bool
isSafe (_,mgs) = null [ ()
  | mg <- mgs, let g = shiftR mg 8, let m = mg .&. 31, g /= 0, m .&. complement g /= 0 ]

p2st :: [Word16] -> State
p2st [ev,m1,g1,m2,g2,m3,g3,m4,g4,m5,g5] = (ev, mgs) where
  mgs = [ fl f | f <- [0..3] ]
  fl f = foldr (.|.) 0
    [ (if m == f then bit i else 0) .|. (if g == f then bit (i+8) else 0)
    | (m,g,i) <- zip3 [m1,m2,m3,m4,m5] [g1,g2,g3,g4,g5] [0..]
    ]

goalState = p2st $ replicate 11 3

initState = p2st $ [0,0,0,2,1,2,1,2,1,2,1]

{-
遷移先の状態は、
evの位置のmgに関して、choicesのビットを全て含むようなものだけ、
そこからはハズして、ev+-1のフロアにorして、
それがisSafeでかつ、調査済みでないところ。
上書き更新マンドクセ。
-}

step :: State -> [State]
step (ev,mgs) =
  [ st1
  | c <- choices
  , c .&. mgs !! fromIntegral ev == c
  , ev1 <- [ev-1, ev+1], 0 <= ev1, ev1 <= 3
  , let mgs1 = set ev1 (mgs !! fromIntegral ev1 .|. c) $ set ev (mgs !! fromIntegral ev .&. complement c) mgs
  , let st1 = (ev1,mgs1)
  , isSafe st1
  ]

set 0 v (_:xs) = v:xs
set k v (x:xs) = x:set (pred k) v xs

{-
では、initStateから始めて、goalStateが出てくるまで一歩ずつ進めよう。
-}

ans1 = compute1 1 [initState] S.empty

compute1 :: Int -> [State] -> S.Set State -> Int
compute1 k [] visited = error (show $ S.size visited)
compute1 k states visited
  | trace (unwords [show k, show $ length states, show $ S.size visited]) False = undefined
  | elem goalState states1 = k
  | True = compute1 (succ k) states1 visited1
  where
    states1 = filter (`S.notMember` visited) $ concatMap step states
    visited1 = foldr S.insert visited states

{-
対象が多くて時間がかかりすぎる。
また例によってJSでやれば終われる規模の問題なのか、
ad-hocなアルゴリズムで問題をといて、その手数を数える問題なのか、
局面のgoalとの差みたいなスコアで優先順序をつけて解くべきなのか、
アプローチが選択できない。
-}

main = print ans1
