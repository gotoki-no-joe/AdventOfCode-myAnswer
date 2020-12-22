import Data.List
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.Array

import System.IO.Unsafe

{-
タイルの大きさ10x10は決まっているらしい
Tileで始まる行で、番号を得る
続く10行でパターンを得る
空行を飛ばす
の繰り返しで、全てのパターンを読み取れる

得た画像は、先頭行、最終行、transposeしたものの先頭行と最終行、
をビットパターンとして取り出す。
ただし、逆順と比べて小さい方を、正規化として選択する。

「他と偶然同じ」ビットパターンは現れない、と仮定する。
辺のビットパターンをキーに、タイル番号のリストを値にしたマップを構築すると、
リストが長さ1のキーは、単独の辺。つまり外周。
リストの長さが2のキーは、対がある。これが4とかだと困る。

そして再度、タイルの方からその各辺で対がないものを数えると、
0 : 中
1 : 外周
2 : 四隅
と分類できる。
-}

test1 = readFile "sample.txt" >>= pure . buildBP2ID . parseAll >>= print

type Tile = (Int,[String])

-- 入力を全て読む
parseAll :: String -> [Tile]
parseAll str = loop ls
  where
    ls = lines str
    loop (l:ls)
      | take 5 l == "Tile " = (read $ take 4 $ drop 5 l, take 10 ls) : loop (drop 11 ls)
    loop _ = []

-- 一行のビットパターンを作る
genPat :: String -> Int
genPat str = min (f ns) (f $ reverse ns)
  where
    g '#' = 1
    g '.' = 0
    f = foldl (\x y -> x+x+y) 0
    ns = map g str

-- Tileの4つのビットパターンを作る
genPatterns :: Tile -> [Int]
genPatterns (_,ls) = map genPat [head ls, last ls, head tls, last tls]
  where
    tls = transpose ls

{-
-- ビットパターンからIDのマップをつくる
buildBP2ID :: [Tile] -> M.IntMap [Int]
buildBP2ID ts = M.fromListWith (++) l
  where
    l = [(bp, [fst t]) | t <- ts, bp <- genPatterns t]
-}

-- ビットパターンからIDのマップをつくり、それにより
-- 対のないビットパターンのIntSetを作る
buildBP2ID :: [Tile] -> S.IntSet
buildBP2ID ts = s
  where
    l = [(bp, [fst t]) | t <- ts, bp <- genPatterns t]
    m = M.fromListWith (++) l
    s = S.fromList $ map fst $ filter (singleton . snd) $ M.assocs m

singleton [_] = True
singleton _ = False

comp1 str = (cs1, product cs1)
  where
    tiles = parseAll str
    tbps = [(fst ti,genPatterns ti) | ti <- tiles]
-- buildBP2ID
    l = [(bp, [i]) | (i,bps) <- tbps, bp <- bps]
    m = M.fromListWith (++) l
    s = S.fromList $ map fst $ filter (singleton . snd) $ M.assocs m
-- 対のない辺の数を数える
    cs = [(i,length [() | bp <- bps, S.member bp s]) | (i,bps) <- tbps]
--  snd=2な要素が4つあることを確認し、idの積を作る
    cs1 = map fst $ filter ((2 ==).snd) cs

test2 = readFile "sample.txt" >>= print . comp1

ans1 = readFile "input.txt" >>= print . comp1

{-
*Main> test1
fromList [24,43,66,78,161,177,231,271,391,481,501,587]
*Main> test2
([1951,1171,2971,3079],20899048083289)
*Main> ans1
([3833,2593,2999,3517],104831106565027)

パート2
言われたとおりのことを計算するだけで、アルゴリズム的な難しさはないんだけど、
色々な要素があってただひたすら面倒で、その面倒を面倒でなくする方面のセンスが求められているのかしら。
Haskellだと対話環境だとはいってもIOの中のことが外に取り出せないから、
結果を変数にいれて手作業でちょっとしたことができなくて微妙だ。

最小のID:2999を手で選んで、どちら向きなのかも手動でしていする。
これに対して下にぶら下がるタイルを
*Main> readFile "input.txt" >>= print . length . parseAll
144
もう11枚、向きも考えて、ビットパターンで見つけて確定させる。
次にこの12枚を転置して、
それぞれの下に11枚ずつぶら下がるタイルを繋げる。

タイルがあるとき、これにぶら下がるタイルは、last と等しいパターンをheadに持つタイル。
ただし自身を除く。というこれが面倒だなまた。
しかし、1で作ったグラフは正規化しているから、IDわかっても向きがわからないから、
結局作り直した方が早いのだ。

なので、タイルに対して、headのパターンをキーとして、そのように回転した内容とIDのリストをバリューとするマップを作る。
そして、下につながるようなタイルの、IDと回転済みの内容のチェーンを構築する。

normal | reverse | map reverse | transpose | r . m r | r . t | m r . t | all
1 2    | 3 4     | 2 1         | 1 3       | 4 3     | 2 4   | 3 1     | 4 2
3 4    | 1 2     | 4 3         | 2 4       | 2 1     | 1 3   | 4 2     | 3 1
map reverse . transpose == rotate なんだ！へー。

そうしたら、周囲をトリムして一枚に結合して、ようやく次のステージに進む。
-}

indata = unsafePerformIO (readFile "input.txt")

tiles0 = parseAll indata

buildBP2ID4 :: [Tile] -> M.IntMap [Tile]
buildBP2ID4 ts = m
  where
    l = [(genPat0 $ head bs1, [(i,bs1)]) | (i,bs) <- ts, bs1 <- vary bs]
    m = M.fromListWith (++) l

-- 一行のビットパターンを作る、正規化なし
genPat0 :: String -> Int
genPat0 str = foldl (\x y -> x+x+ g y) 0 str
  where
    g '#' = 1
    g '.' = 0

vary0 = id
vary1 = reverse
vary2 = map reverse
vary3 = transpose
vary4 = vary1 . vary2
vary5 = vary1 . vary3
vary6 = vary2 . vary3
vary7 = vary1 . vary2 . vary3

vary bs = map ($ bs) [vary0, vary1, vary2, vary3, vary4, vary5, vary6, vary7]

bp2id4 = buildBP2ID4 tiles0

startTile :: Tile
startTile = (2999, vary2 r)
  where
    Just r = lookup 2999 tiles0

hangsTo :: Tile -> [Tile]
hangsTo t = t : unfoldr step t
  where
    step (tid,bs)
      | null tl = Nothing
      | True = Just (t,t)
      where
        tl = filter ((tid /=).fst) $ (bp2id4 M.!) $ genPat0 $ last bs
        t = head tl

{-
hangsTo :: Tile -> Tile
hangsTo (tid,bs) = head $ filter ((tid /=).fst) $ (bp2id4 M.!) $ genPat0 $ last bs
-}

-- (2999,vary2 startTile) と (2999,vary3 $ vary2 startTile) がhangsToを同じだけ持つので、これが正しい向き。

{-
*Main> length $ hangsTo startTile
12
*Main> length $ hangsTo $ fmap vary3 startTile
12
-}

allTiles = transpose $ map hangsTo $ map (fmap vary3) $ (hangsTo startTile)

trim bs = trim1 (map trim1 bs)
  where
    trim1 xs = init (tail xs)

bigPicture = concatMap (foldl1 (zipWith (++)) . map (trim . snd)) allTiles

{-
*Main> length bigPicture
96
*Main> length (head bigPicture)
96
*Main> length $ filter ('#' ==) $ concat bigPicture
2648
-}

{-
Sea Monsterを探すには、Array Boolを作るのでいいか。もう。
20x3のパターンだ。

                  # 
#    ##    ##    ###
 #  #  #  #  #  #

15pixある。
-}

seaMonster ba x y = and $ map (ba !) ps
  where
    ds = [(18,0),(0,1),(1,2),(4,2),(5,1),(6,1),(7,2),(10,2),(11,1),(12,1),(13,2),(16,2),(17,1),(18,1),(19,1)]
    ps = [ (x+dx,y+dy) | (dx,dy) <- ds ]

countSM pic = length [() | y <- [1..w-2], x <- [1..w-19], seaMonster ba x y]
  where
    w = length pic
    ba = listArray ((1,1),(w,w)) $ map ('#' ==) $ concat pic

sample2 = lines $ unsafePerformIO (readFile "sample2.txt")

test3 = map countSM $ vary sample2

{-
*Main> map countSM $ vary bigPicture 
[0,0,0,0,0,0,0,37]
*Main> 2648 - 15 * 37
2093

一つのプログラムにまとめる元気がないわ。
23:39:01   6694
時間ぎりぎり。
-}
