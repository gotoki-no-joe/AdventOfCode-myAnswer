import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Either

main = exec "input.txt"
test = exec "test.txt"

exec fn = do
  fi <- readFile fn
  let ls = map parse $ lines fi
  let parent = buildParent ls
  let nodes = map (\(n,_,_) -> n) ls
  let r = theirRoots nodes parent
  print r
  let tow = towerWeight ls
  reportUnbalance ls tow
  let root = fst (head r)
  let children = M.fromList [ (p,cs) | (p,_,cs) <- ls ]
  let weights = M.fromList [ (p,w) | (p,w,_) <- ls ]
  let ans2 = repairTower children weights tow root undefined
  print ans2

{-
今度こそパーサがほしい気がする。
でもまだ何とかなるか。
Data.List.Split.splitOn 様々。
-}

parse :: String -> (String,Int,[String])
parse cs = (name,read weight,names) where
  (name, ' ':'(':cs1) = break (' ' ==) cs
  (weight, cs2) = break (')' ==) cs1
  names = filter (not.null) $ splitOn ", " $ drop 5 cs2

{-
ノードの名前から、その親の名前をひくMapを作る。
-}

buildParent :: [(String,Int,[String])] -> M.Map String String
buildParent ls = M.fromList [ (c,p) | (p,_,cs) <- ls, c <- cs ]

{-
全てのノードについて、親を辿った先を求める。
-}

theirRoots :: [String] -> M.Map String String -> [(String,Int)]
theirRoots nodes parent =
  map (\ns -> (head ns, length ns)) $ group $ sort $ map root nodes where
    root n
      | M.member n parent = root (parent M.! n)
      | otherwise = n

{-
まずは動的プログラミングで、全サブタワーの重量を作ろう。
-}

towerWeight :: [(String,Int,[String])] -> M.Map String Int
towerWeight ls = tow where
  tow = M.fromList [ (n, w + sum [tow M.! c | c <- cs] ) | (n,w,cs) <- ls ]

{-
バランスしていない親子関係を表示してみようか。
…3つだけだったから、手でも何とかなるが。
-}
reportUnbalance :: [(String,Int,[String])] -> M.Map String Int -> IO ()
reportUnbalance ls tow = mapM_ report ls where
  report (n,w,cs) = do
    let cws = map (tow M.!) cs
    if null cs || all (head cws ==) cws then return () else print n

{-
元々再帰的にバランスしているところに、誰か一人だけ重量を変えられている。
すると、そこから下は全てバランスしなくなる。打ち消すことはない。
なので、根から探していけばそのノードはかならず見つかる。

底から始めると、自分の子全ての重量が一人だけ違うはず。
（そして他は全て等しい上に、バランスしているはず…はチェックする？）

一人違うものを、「この体重に合わせなさい」と再帰的に調べる。
子のうちの一人が違う体重のとき、同様に再帰する。
子が全て同じ体重のとき、自分の体重を修正することで答がでる。
-}

repairTower :: M.Map String [String] -> M.Map String Int -> M.Map String Int -> String -> Int -> (String, Int)
repairTower children weights tow node w
  | null cs = (node, w) -- こんなところまで降りてくることはないが。
  | all (w0 ==) cws = (node, w - sum cws) -- 子がバランスしているならここが原因
  | otherwise = repairTower children weights tow c cw -- バランスしていない子を再帰的に調べる
  where
    cs = children M.! node
    cws = map (tow M.!) cs
    w0 = head cws
    (as,bs) = partitionEithers [ f (c,cw) | (c,cw) <- zip cs cws, let f = if w0 == cw then Left else Right ]
    (c,cw) = if singleton as then (fst $ head as,snd $ head bs) else (fst $ head bs,snd $ head as)

singleton [_] = True
singleton _ = False

{-
仲間はずれの体重をもつ子を特定するの、すげぇ面倒くさいな。
-}

{-
*Main> main
[("azqje",1083)]
"nzeqmqi"
"inwmb"
"azqje"
("rfkvap",646)
-}
