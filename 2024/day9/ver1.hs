import Data.Char
import Data.List

import qualified Data.Sequence as Q
import Data.Foldable

import Debug.Trace

sample = "2333133121414131402"

runner f = readFile "input.txt" >>= print . f . head . lines

test1 = part1 sample
main1 = runner part1

{-
入力は多分奇数文字なんだろう。
愚直に配列でごちゃごちゃやるのがいいのか？
前からと、後ろからの列を使って、手ループでなんとかならんかな。
終わりが見えないか。位置情報を持っていないと。
それを持たせると、配列と同じな気もするが。

それぞれのブロックの位置とファイルIDの対をリストにする。
前と後ろからこのリストを見て、
前が詰まっていれば前を出力する
空いていれば、後ろの最後のものを出力して空きを捨てる
前と後ろが同じブロック位置を見たところで終わる。
-}

part1 :: String -> Int
part1 l = sum $ zipWith (*) [0 ..] $ consume ijs $ reverse ijs
  where
    ijs = zip [0 ..] $ genFile 0 l
    genFile fid (c:cs) = replicate (digitToInt c) fid ++ genSpace (succ fid) cs
    genSpace fid (c:cs) = replicate (digitToInt c) (-1) ++ genFile fid cs
    genSpace _ [] = []
    consume ijijs@((i,j):ijs) pqpqs@((p,q):pqs)
      | i == p, j == -1 = []
      | i == p, j /= -1 = [j]
      | j == -1, q /= -1 = q : consume ijs pqs
      | j == -1, q == -1 = consume ijijs pqs
      | otherwise = j : consume ijs pqpqs

{-
ghci> test1
1928
ghci> main1
6337367222422
-}

{-
ファイルを見たら20000文字あった。

今度はどんなデータ構造がいけてる？
空き領域は最終的に0になって消えるまで、減る一方。
動かそうとしているファイルより後ろのをうっかり指定しないように注意。
ファイルID/空き と長さ、のリストに書き換えて、全体の書き換えをやると、2e4^2のオーダーでかなり重いがまだ行ける。

移動させたブロックはただ消すのでなくて、空間を空けた上で、前後と接続する必要があるな。
-}

test2 = part2 sample
main2 = runner part2

part2 :: String -> Int
part2 l = sum $ zipWith (*) [0 ..] $ inflate ilsZ
  where
    ils0 = zip (intersperse (-1) [0 ..]) (map digitToInt l)
    imax = fst $ last ils0
    ilsZ = foldl' step ils0 [imax, pred imax .. 1]
    inflate = concatMap (\(a,b) -> replicate b (max 0 a))
    step ils fid = --traceShowId $
      case break prop abs of
        (_, []) -> ils
        (abs1, (a,b):abs2) | q == b -> abs1 ++ (p, q) : reg (abs2 ++ (-1,q) : pqs) -- a == -1
                        | otherwise -> abs1 ++ (p, q) : (a, b - q) : reg(abs2 ++ (-1,q) : pqs)
      where
        (abs,(p, q):pqs) = break ((fid ==) . fst) ils -- p == fid
        prop (a,b) = a == -1 && q <= b
    reg ((-1,a):(-1,b):ils) = reg ((-1,a+b):ils)
    reg (il:ils) = il : reg ils
    reg [] = []

main = do
  print test1
  main1
  print test2
  main2b

{-
1928
6337367222422
2858
6361380647183

インタプリタだと終わりが見えなかった。まぁ力任せだね。
答えは出たので、もっと賢くやりたい。
区間を表すために、開始位置と幅、をIntMapで持つやつがあるけど、
「空きの範囲」も求めないといけないのがね。
なのでちょっと違う。挿入削除前後移動が簡単なのは、二重リンクリストかしら。命令型なら。

壊せない両端、を追加した Data.Sequence を使うのがいいかしらね。

んー、空いた空間というのは、とあるファイルfidを動かした結果にできるもの。
それ以降に動かすのはファイルfidより小さいものだから左にあって、連続性を取り返す必要はない！
なぁんだ。なのでregを消しても問題なかった。そしてそうしたらtraceつきで待てる時間になった。
まだ時間掛かるけど。

さらに、あるfidより後ろは、これ以降はもう使わないから、「残り」としてハズしておけばよくない？と。
-}

test2a = part2a sample
main2a = runner part2a

part2a :: String -> Int
part2a l = sum $ zipWith (*) [0 ..] $ inflate ilsZ
  where
    ils0 = zip (intersperse (-1) [0 ..]) (map digitToInt l)
    imax = fst $ last ils0
    ilsZ = uncurry (++) $ foldl step (ils0,[]) [imax, pred imax .. 1]
    inflate = concatMap (\(a,b) -> replicate b (max 0 a))
    step (ils,rest) fid = -- traceShow fid $
      case break prop abs of
        (_, []) -> (ils, rest)
        (abs1, (a,b):abs2) | q == b -> (abs1 ++ (p, q) : abs2, (-1,q) : pqs ++ rest) -- a == -1
                        | otherwise -> (abs1 ++ (p, q) : (a, b - q) : abs2, (-1,q) : pqs ++ rest)
      where
        (abs,(p, q):pqs) = break ((fid ==) . fst) ils -- p == fid
        prop (a,b) = a == -1 && q <= b

-- 後ろからならSequenceでしようね

test2b = part2b sample
main2b = runner part2b

part2b :: String -> Int
part2b l = sum $ zipWith (*) [0 ..] $ inflate $ toList ilqZ
  where
    ilq0 = Q.fromList $ zip (intersperse (-1) [0 ..]) (map digitToInt l)
    imax = fst $ lastQ ilq0
    ilqZ = loop ilq0 imax
    inflate = concatMap (\(a,b) -> replicate b (max 0 a))
    loop ilq fid
      | fid == 0 = ilq
      | otherwise = -- traceShow fid $
          case Q.breakl prop abq of
            (_, Q.Empty) -> loop abq (pred fid) Q.>< ((a,b) Q.:<| cdq)
            (abq1, (x,y) Q.:<| abq2) -> loop (abq1 Q.>< (a,b) Q.:<| (-1, y-b) Q.:<| abq2) (pred fid) Q.>< ((-1,b) Q.:<| cdq)
      where
        (cdq, abq Q.:|> (a,b)) = Q.breakr ((fid ==) . fst) ilq
        prop (x,y) = x == -1 && b <= y

lastQ (_ Q.:|> x) = x
