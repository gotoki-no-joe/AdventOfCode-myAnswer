import Data.Char
import Data.List.Split
import qualified Data.Map as M
import Data.Array
import qualified Data.IntSet as IS

import Data.List

import Debug.Trace

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

part12 ls = (ans1, ans2a) -- (ans1, ans2)
  where
-- 並び順で背番号を付ける
    name2id = M.fromList [(vv, i) | ((vv,_,_),i) <- zip ls [1 ..]]
    n = length ls
-- 有向グラフとrateの配列
    rate = listArray (1,n) [r | (_,r,_) <- ls]
    g = listArray (1,n) [map (name2id M.!) uus | (_,_,uus) <- ls]

{-
現在位置の頂点と、開放した、またはスルーした頂点の集合をキーに、
その構成で獲得できる最大スコアを値として保持するDPを行う。
としたいが、移動とバルブ開放にそれぞれ時間を消費するので、モデル化が難しいな。
rate=0の頂点は、移動した瞬間に開放扱いか、初期状態で開放扱いでいいだろうか。
rate>0の頂点については、移動直後は「開放済み」でも「無視済み」でもなくて、
1tick消費することで、位置を変えずに開放になるか、
開放せずに次の位置に進むことで無視になるか、どちらか、とすればできるだろうか。

持ち時間は30minしかないので、本番データ60頂点は、全て巡ることはできない。
64ビット整数のビット集合で扱うのはスカスカすぎるし広すぎるので、
形式化を重視してIntSetとMapで。
-}
-- 初期状態は "AA" にいて、rate=0のバルブを全開放していて、スコア0
    state0 = M.singleton (name2id M.! "AA", IS.fromList [i | (i, 0) <- assocs rate]) 0
-- 30ステップ遷移させる
    state30 = foldr step state0 [1 .. 29]
    ans1 = maximum $ M.elems state30
-- それぞれの状態について
-- 未解放のとき、移動せずバルブ開放できる。このときスコアを、rate*残り時刻だけ増やす。
-- 開放のいかんに拘わらず、隣接頂点に移動できる。このとき、無視したとしてバルブはチェックを入れる。
    step time state = res
      where
        res = M.fromListWith max $ concatMap (stepFunc time) $ M.assocs state
    stepFunc time ((i, js), score)
      | IS.notMember i js = valveOpen : moves
      | otherwise         = moves
      where
        js1 = IS.insert i js
        valveOpen = ((i, js1), score + time * rate ! i)
        moves = [((j, js), score) | j <- g ! i]
-- 1足らないんだけど
    id2name i = let (n,_,_) = ls !! pred i in n
    ansView = [(id2name i, map id2name $ IS.elems js,s) | ((i,js),s) <- M.assocs state30, s == ans1]
    stView m = [(id2name i, map (head . id2name) $ IS.elems js,s) | ((i,js),s) <- M.assocs m]
    maxStView m = [(id2name i, map (head . id2name) $ IS.elems js,s) | ((i,js),s) <- M.assocs m, s == smax]
      where
        smax = maximum $ M.elems m

{-
説明によると、残りタイム×レート で
28 * 20 + 25 * 13 + 21 * 21 + 13 * 22 + 9 * 3+ 6 * 2 = 1651
という計算らしい。

引き返して開ける、アリなんだ。直った。

そしてパート2

持ち時間が4減る代わりに、エージェントが2人になると。
別々にシミュレーションかける代わりに、単独シミュレーションの時刻26の結果について、
開放バルブの重複が0バルブのみ、という者同士を突き合わせて、合計したらいいだけなのでは？
-}

    ztate0 = M.singleton (name2id M.! "AA", IS.empty) 0
    ztate26 = foldr ztep ztate0 [1 .. 25]
    ztep time state = res
      where
        res = M.fromListWith max $ concatMap (ztepFunc time) $ M.assocs state
    ztepFunc time ((i, js), score)
      | openable  = valveOpen : moves
      | otherwise = moves
      where
        js1 = IS.insert i js
        valveOpen = ((i, js1), score + time * rate ! i)
        moves = [((j, js), score) | j <- g ! i]
        openable = rate ! i > 0 && IS.notMember i js

    z26ents = M.assocs ztate26
    ans2 = maximum
      [ s1 + s2
      | ((_,js1),s1):sts <- tails z26ents
      , ((_,js2),s2) <- sts
      , IS.disjoint js1 js2
      ]

{-
時刻26の状態数を数えたら 90706 個だった
90706 * 90705 `div` 2 = 4,113,743,865
ちょっとインタプリタには重い数だったかな。

コンパイルしても重いわ。と思ったら出た。

> ghc -O2 ver2
> ./ver2
(1651,1707)
(2253,2838)

正解だったからもういいんだけど、微妙に引っかかるね。
状態を圧縮するのに、
・同じシチュエーションの中でスコア最大のものだけ、は簡単にできるし必須だしDPとはそれだからそうしたけど、
・単調性、開けたバルブがもっと少ないのにスコアは上のものがあったら無意味だから捨てる
　開けたバルブがもっと多いのにスコアは下なのは捨てる
を、一つ挿入するたびに調べると大変なのでできない。んじゃないかな。

10回に1回だけそれをして正規化する、みたいな計算の仕方はできるのかな。

開けたバルブ集合の包含関係、だと 2^|S| 通りで辛いけど、
スコアを昇順に並べて前から順に見て、
自分より安いのに集合が大きいものは捨てる、は面倒。
スコアを降順に見て、
自分より高くて集合が小さいものがもうあったらそれは入れない、ならやりやすい。
パート2専用なら、最終位置も潰して縮小できるけど、どうしようか。
パート1の高速化も狙えるかと思ったのだけど。
-}

-- 不要な要素を外してみる
    z26ents1 =
      foldl regstep [] $ -- 要らないものは除いて
      sortBy (\(s1,_) (s2,_) -> compare s2 s1) $ -- スコアの降順に整列して
      map (\((_,js),s) -> (s,js)) z26ents -- 最終位置を捨てて順番変えて
    regstep es e@(s,js)
      | cond = es
      | otherwise = e:es
      where
        cond = any (\(_,jt) -> IS.isSubsetOf jt js) es

{-
(length z26ents, length z26ents1)

ghci> test1
(320,64)
ghci> main1
(90706,1779)
ここまで放置していたからか、圧縮効果高いな。毎回やるとそれはそれでオーバーヘッドでしょこれ。
ていうか、最終位置を捨てたせいか。
-}

    ans2a = maximum
      [ s1 + s2
      | (s1,js1):sjs <- tails z26ents1
      , (s2,js2) <- sjs
      , IS.disjoint js1 js2
      ]

-- パート2の結果だけなら、インタプリタで余裕で待てる時間で出たよ。
-- あれま。
-- こうなると、通常のステップにこの正規化を盛り込んで見たくなるねぇ？
-- 同じ構成でスコア最大のものだけ残す、というロジックも入っているから、MapとかfromListWith maxとか不要になるんだな。
-- もう別物じゃんそれ。
