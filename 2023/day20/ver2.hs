{-
記憶はNANDモジュール側でなく、各出力モジュールの側で行う。
inputでも58行しかないので、行番号を使ったArrayでネットワークを表す

モジュールは送信先を複数持つので、それらにパルスを送信する、それをキューで管理する

モジュールは受信元を複数持つ。NANDモジュールは動作のためにそれを持っている必要がある。
FFモジュールなどは受信元を一つしか持たないはずだが、どうだろう。
そんなことなかった。ただ、今届いたパルスに関してのみ動けばよいので、接続元が誰かはあまり気にしなくてよい。

NANDモジュールを、説明どおりにモジュール側で記憶するより、受信元だけ知っておいて、それを調べる方が情報が分散しないのでいいはず。
-}
import qualified Data.Map as M
import Data.Char
import Data.Array
import qualified Data.Sequence as Q
import Data.List

import Control.Monad
import Data.Array.IO
import System.IO

runner i f = do
  ls <- map parse . lines <$> readFile i
  print $ f ls

parse l = (s1, map init (init ss) ++ [last ss])
  where
    s1:_:ss = words l

test1 = runner "samp1.txt" part1
main1 = runner "input.txt" part1

part1 ls0 = (cntH, cntL, cntH * cntL)
  where
    ls = ls0 ++ [("rx", [])] -- 内部接続がない信号
    n = length ls
-- モジュール名から背番号へのマップ
    s2i = M.fromList [(ss, i) | (i, (s,_)) <- zip [1 ..] ls, let ss = if elem (head s) "%&" then tail s else s]
-- モジュール番号に対して、出力先の出力順リストな配列
    output = listArray (1,n) [map (s2i M.!) ss | (_,ss) <- ls]
-- モジュールに入力する送信元のリストな配列、NAND用
    input = accumArray (flip (:)) [] (1,n) [(j,i) | (i,js) <- assocs output, j <- js]
-- モジュール番号から、それがNANDか、FFかの表
    isNAND = listArray (1,n) [head s == '&' |(s,_) <- ls]
    isFF = listArray (1,n) [head s == '%' | (s,_) <- ls]
-- キューにはいっているイベントを順にシミュレーションするステップ、各モジュールの出力状態を変更する
-- でなくて、キューが空になるまで次々回して、消費したHパルス、Lパルスの個数と状態を返す
    eventLoop cntH cntL st Q.Empty = (cntH, cntL, st)
    eventLoop cntH cntL st ((i, high) Q.:<| q)
      | isFF ! i, high   = eventLoop cntH1 cntL  st q -- FFへのHパルスは無視される
      | isFF ! i         = eventLoop cntH  cntL1 (st // [(i, nstI )]) (q Q.>< send nstI )
      | isNAND ! i       = eventLoop cntH1 cntL1 (st // [(i, nallH)]) (q Q.>< send nallH)
      | otherwise        = eventLoop cntH1 cntL1 (st // [(i, high )]) (q Q.>< send high ) -- broadcaster
      where
        cntH1 = if high then succ cntH else cntH
        cntL1 = if high then cntL else succ cntL
        stI = st ! i
        nstI = not stI
        send x = Q.fromList [(o,x) | o <- output ! i]
        allH = and [st ! j | j <- input ! i]
        nallH = not allH
-- 初期状態から1000回回す
    ini = listArray (1,n) $ repeat False
    (cntH, cntL, _st) = foldl' step (0, 0, ini) [1 .. 1000]
    step (cH, cL, st) _ = eventLoop cH cL st iniq
    iniq = Q.singleton (s2i M.! "broadcaster", False)

{-
ghci> test1
(4000,8000,32000000)
ghci> main1
(50633,19364,980457412)

パート1正解するところまで再現できたけれど、これおかしいよ。
NANDは全ての入力を覚えておいて、その記憶の上でオールHならLだ、というのが定義。
この実装は、何かパルスが来た時点で接続先の状態を見て、それがオールHならLにしている。
そしてパルスは、キューに入って届くので、届くまでに遅れがある。
パルス1 : 入力1の元がHを送信
パルス2 : 入力2の元がLだったのがHに変わって送信
パルス1がNANDに到着、このとき記憶はオールHではないのに、読み出し方式だとそうなってしまう。
OMG!これが許される優しいネットワークなのか、ミスなのか。
ボタンを押すところから続けるとき、NANDがリセットされるミスを防ぐために、そういう配慮がしてあると信じよう。

そして、ブン回す以外にこの答えを見つける方法が思い当たらない。
記号実行で2^58通りのネットワークが解明できる？
時計を逆回しにする？

初期状態が不明、という状態から開始して、
ある信号を初めて参照したところで、それがFalseだったらとTrueだったらの両方のインスタンスを追跡することにして、
rxに信号出力をするか、キューが空になるまで完了したら、
有意な初期状態とその終了状態（変化しないものも含む）、の対が取り出せて、
全て出てきたか、リセット状態からゴール状態までが到達可能になったら、その距離が答えなんだが。
-}

main2 = runner "input.txt" part2

part2 :: [(String, [String])] -> Int
part2 ls0 = ans2
  where
    ls = ls0 ++ [("rx", [])] -- 内部接続がない信号
    n = length ls
-- モジュール名から背番号へのマップ
    s2i = M.fromList [(ss, i) | (i, (s,_)) <- zip [1 ..] ls, let ss = if elem (head s) "%&" then tail s else s]
-- モジュール番号に対して、出力先の出力順リストな配列
    output = listArray (1,n) [map (s2i M.!) ss | (_,ss) <- ls]
-- モジュールに入力する送信元のリストな配列、NAND用
    input = accumArray (flip (:)) [] (1,n) [(j,i) | (i,js) <- assocs output, j <- js]
-- モジュール番号から、それがNANDか、FFかの表
    isNAND = listArray (1,n) [head s == '&' |(s,_) <- ls]
    isFF = listArray (1,n) [head s == '%' | (s,_) <- ls]
-- キューにはいっているイベントを順にシミュレーションするステップ、各モジュールの出力状態を変更する
-- でなくて、キューが空になるまで次々回して、消費したHパルス、Lパルスの個数と状態を返す
    eventLoop st Q.Empty = Just st
    eventLoop st ((i, high) Q.:<| q)
      | isFF ! i, high   = eventLoop st q -- FFへのHパルスは無視される
      | isFF ! i         = eventLoop (st // [(i, nstI )]) (q Q.>< send nstI )
      | isNAND ! i       = eventLoop (st // [(i, nallH)]) (q Q.>< send nallH)
      | i == s2i M.! "rx", not high = Nothing
      | otherwise        = eventLoop (st // [(i, high )]) (q Q.>< send high ) -- broadcaster
      where
        stI = st ! i
        nstI = not stI
        send x = Q.fromList [(o,x) | o <- output ! i]
        allH = and [st ! j | j <- input ! i]
        nallH = not allH
-- 初期状態からひたすら回し、Nothingが出たら終わる
    ini = listArray (1,n) $ repeat False
    ans2 = loop 1 ini
    loop cnt st =
      case eventLoop st iniq of
        Nothing  -> cnt
        Just st1 -> loop (succ cnt) st1
    iniq = Q.singleton (s2i M.! "broadcaster", False)

-- ブン回しをするならせめてmutable array化して、コンパイルして動かそう。

part2a :: [(String, [String])] -> IO Int
part2a ls0 = ans2
  where
    ls = ls0 ++ [("rx", [])] -- 内部接続がない信号
    n = length ls
-- モジュール名から背番号へのマップ
    s2i = M.fromList [(ss, i) | (i, (s,_)) <- zip [1 ..] ls, let ss = if elem (head s) "%&" then tail s else s]
-- モジュール番号に対して、出力先の出力順リストな配列
    output = listArray (1,n) [map (s2i M.!) ss | (_,ss) <- ls]
-- モジュールに入力する送信元のリストな配列、NAND用
    input = accumArray (flip (:)) [] (1,n) [(j,i) | (i,js) <- assocs output, j <- js]
-- モジュール番号から、それがNANDか、FFかの表
    isNAND = listArray (1,n) [head s == '&' |(s,_) <- ls]
    isFF = listArray (1,n) [head s == '%' | (s,_) <- ls]
-- キューにはいっているイベントを順にシミュレーションするステップ、各モジュールの出力状態を変更する
-- でなくて、キューが空になるまで次々回して、消費したHパルス、Lパルスの個数と状態を返す
    eventLoop st Q.Empty = return False
    eventLoop st ((i, high) Q.:<| q)
      | isFF ! i, high   = eventLoop st q -- FFへのHパルスは無視される
      | isFF ! i         =
          do
            nstI <- not <$> readArray st i
            writeArray st i nstI
            eventLoop st (q Q.>< send nstI)
      | isNAND ! i       =
          do
            nallH <- not . and <$> forM (input ! i) (readArray st)
            writeArray st i nallH
            eventLoop st (q Q.>< send nallH)
      | i == s2i M.! "rx", not high = return True
      | otherwise        =
          do
            writeArray st i high
            eventLoop st (q Q.>< send high) -- broadcaster
      where
        send x = Q.fromList [(o,x) | o <- output ! i]
-- 初期状態からひたすら回し、Nothingが出たら終わる
    ans2 = do
      st <- newArray (1,n) False :: IO (IOUArray Int Bool)
      loop <- fixIO $ \loop -> return $ \cnt -> do
        res <- eventLoop st iniq
        when (mod cnt 1000000 == 0) (print cnt)
        if res then return cnt else loop $! succ cnt
      loop 1
    iniq = Q.singleton (s2i M.! "broadcaster", False)

main = do
  ls <- map parse . lines <$> readFile "input.txt"
  res <- part2a ls
  print res

