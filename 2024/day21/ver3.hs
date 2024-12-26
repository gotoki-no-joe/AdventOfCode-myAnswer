import qualified Data.Map as M
import Data.List.Split

import Data.IORef
import System.IO.Unsafe

{-
https://todd.ginsberg.com/post/advent-of-code/2024/day21/
を写経する
-}

-- テンキーパッドと方向キーパッドを表現する

numericPad, directionalPad :: M.Map (Int,Int) Char
numericPad = M.fromList
  [ ((i,j),c)
  | (i, cs) <- zip [0 ..] $ chunksOf 3 "789456123*0A"
  , (j, c ) <- zip [0 ..] cs, c /= '*' ]
directionalPad = M.fromList
  [ ((i,j),c)
  | (i, cs) <- zip [0 ..] $ chunksOf 3 "*^A<v>"
  , (j, c ) <- zip [0 ..] cs, c /= '*' ]

{-
両方のキーパッドについて、全てのボタンの間の最短経路を求める。
二つある場合は両方求める。（複数あるときは全て求める。）
-}

numericPaths, directionalPaths :: M.Map (Char,Char) [String]
numericPaths = allPaths numericPad
directionalPaths = allPaths directionalPad

{-
kotlinのコードは、メソッドを貼り付ける対象の型の名前と、
メソッドの結果の値に挟まれて、どこに関数名が書いてあるのかを見つけるのがまず一苦労だなぁ。

allPathsはまず、
allPairsで map の key の二つ組を総当たりで作り、
associateの中のラムダで、それを引数に findLowestCostPaths をした結果を
二つ組みに対応付けるmapを作っている。
-}

allPaths pad = M.fromList [((pad M.! s, pad M.! e), map (reverse . ('A' :)) p) | ((s,e),p) <- M.assocs m ]
  where
    m = M.fromDistinctAscList
      [((s,e), findLowestCostPaths s e) | s <- M.keys pad, e <- M.keys pad]
    findLowestCostPaths s e
      | s == e = [""]
      | otherwise = shortests [d : p | (n,d) <- closer s e, p <- M.findWithDefault [] (s,n) m]
-- (p,q) より (i,j) に近い (p,q) の隣接点と、そこから(p,q)への移動コマンド
    closer (i,j) (p,q) =
      [((pred p, q), 'v') | i < p] ++ [((succ p, q), '^') | p < i] ++
      [((p, pred q), '>') | j < q] ++ [((p, succ q), '<') | q < j] -- その隣接点からここ(i,j)に移動するコマンドを追加するので、なんか逆に見える

-- いちばん短いものを全て
shortests0 xs = loop maxBound [] xs
  where
    loop _ acc [] = acc
    loop len acc (x:xs) =
      case compare len lx of
        LT -> loop len acc xs
        EQ -> loop len (x:acc) xs
        GT -> loop lx  [x] xs
      where
        lx = length x

shortests xs = snd $ foldr step (maxBound, []) xs
  where
    step x (len, acc) =
      case drop (pred len) x of
        []  -> (length x, [x])
        [_] -> (len, x : acc)
        _   -> (len, acc)

{-
画面いっぱいのKotlinコードでつらい。読む気がしない。
BFSで探しているっぽいねぇ。
もっといつものようにHaskellらしく(？)できないかな。

マンハッタン距離が自分より近い隣接点1つまたは2つに対してそのやり方を聞き、
それに、自分からそこに移動する経路を追加して、短いものを抜き出す感じ。だね。

そしてこれは、そのパッドの、というか、そのパッドを前にしているロボについて、
そのアームを移動させるための操作系列を作るという話だな、多分。
-}

{-
次、findCost 関数の話。
コードの連続する2文字が 1,2 だったとき、つまり1を押した後で2を押したいとき、
numericPaths を探して、でもなくて ">" をしたいとなる。
引数は
・code 入力したいコード
・depth ロボの重なる段数、つまり再帰呼び出しの残り回数
・transitions デフォルトはnumericPaths
・キャッシュ、は外付けにしたいよな。キーはcodeとdepthで、だと。

transitionsは最初はnumeric,次からはdirectional

読み飛ばしてたところがあった。
位置から位置で経路を作った後、その位置のキーラベルに貼り替える。ことで、どう操作するのかを取り出せるようにする。
さらに、最後にAを付けて、
位置 s にいる状態から位置 e に移動してその e を入力する系列、にする。ほむ。

-}


memoize :: Ord d => ((d -> a) -> (d -> a)) -> (d -> a)
memoize mf = f
  where
    memo = unsafePerformIO $ newIORef $ M.empty
    f x = unsafePerformIO $ do
      m <- readIORef memo
      case M.lookup x m of
        Just a -> return a
        Nothing -> do
          let a = mf f x
          modifyIORef' memo (M.insert x a)
          return a

{-
findCostは実質、深さごとに別関数なので、memoizeでmfは使わず、明示的に他で作った一つ深い関数を呼び出すようにする。

code :: String と transitions に対して、

codeだけがキーで、キャッシュされていればそれを返す。
'A':code と code をzipしたそれぞれの対に対して、transitionsで操作列群を取り出し、
depth==0な人は、それらの長さの最小値を返すだけ
そうでない人は、次の段のfindCostにこれを聞いた結果の最小値を返し

その総和を答えとする。キャッシュもする。
-}

findCost0 :: String -> Int
findCost0 = length

findCost :: M.Map (Char,Char) [String] -> (String -> Int) -> (String -> Int) -> (String -> Int)
findCost transitions findCostP _rec code =
  sum $ map (minimum . map findCostP . (transitions M.!)) $ zip ('A' : code) code


runner :: Show a => FilePath -> ([String] -> a) -> IO ()
runner i f = do
  ls <- lines <$> readFile i
  print $ f ls

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 ls = sum [findCost3 l * read (init l) | l <- ls]
  where
    findCost3 = memoize (findCost numericPaths findCost2)
    findCost2 = memoize (findCost directionalPaths findCost1)
    findCost1 = memoize (findCost directionalPaths findCost0)

part2 ls = sum [findCost26 l * read (init l) | l <- ls]
  where
    findCost26 = foldr step findCost0 $ numericPaths : replicate 25 directionalPaths
    step t f = memoize (findCost t f)

main2 = runner "input.txt" part2

-- キャッシュを一つにまとめるバージョンも書いてみる？
-- 性能悪化しそうだけど。

newFindCost depthFrom code = memoize findCostSrc (code, depthFrom)
  where
    findCostSrc recur (code, depth)
      | depth == 0 = length code
      | otherwise  = sum
        [ minimum [recur (tr, pred depth) |  tr <- transitions M.! ab]
        | ab <- zip ('A' : code) code]
      where
        transitions = if depth == depthFrom then numericPaths else directionalPaths

part2a ls = sum [newFindCost 26 l * read (init l) | l <- ls]

main2a = runner "input.txt" part2a
