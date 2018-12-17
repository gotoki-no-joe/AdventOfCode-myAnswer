import Data.List
import qualified Data.Map as M

{-
なんかかっちょいいグラフアルゴリズムが必要なのかと身構えたけど、
greedyにやって最小のものが真っ先に見つかる保証ないし、
そもそも小さい順に生成することすらおぼつかないので、
総当たりでやってしまえということになった。
大した数ではないし。

データみると、無向グラフということでいいのかな？運賃で無くて距離だし。
-}

main = do
  fi <- readFile "input.txt"
  let ls = map parse $ lines fi
  let stars = getAllStars ls
--  let ans1 = compute1 ls
--  print ans1
--  print stars
  let table = makeMap ls
--  print table
  let paths = allPath table stars
  let ans1 = minimum paths
  print ans1
  putStrLn "part2"
  let ans2 = maximum paths
  print ans2

parse :: String -> (String,String,Int)
parse cs0 = (as,bs,read ds) where
  (as,cs1) = break (' ' ==) cs0
  (bs,cs2) = break (' ' ==) (drop 4 cs1)
  ds = drop 3 cs2

getAllStars ls = nub [ s | (a,b,_) <- ls, s <- [a,b] ]

{-
総当たりでするときに、
停止条件を調べると同時に枝刈りになる気がする。
選択した航路に現れる星の名前の出現回数をそれぞれ数えたとき、
1が2回2がn-2回になっていたら完成。
そのリストに3以上の数が現れたら放棄。

と思ったら、そう単純でもないのか。
a-b と c-d-e-f-c でもそうなるから、
連結であることを確認しないといけない。どうやって？

運賃表の書き方にとらわれず、どっち向きに移動するのかを
意識して経路を構築して、出現回数が0が1こ、n-1こが1とか。
いやこれでも上のは見分けられない。ぐぬぬ。

ノードの選択を n! の組み合わせで全探索して、その経路の値段を見るのが早いのか。
そのための検索に都合のいいデータ構造は？
node -> [(node,cost)] な map か。
-}

makeMap :: [(String,String,Int)] -> M.Map (String,String) Int
makeMap ls = M.fromList
  [ pair | (a,b,v) <- ls, pair <- [((a,b),v), ((b,a),v)] ]

{-
単純に全探索する
現状の最安値に途中で到達した場合は枝刈りしたいけれど、
そうすると順序のある計算になるよな。
-}

allPath table stars = start where
  start = [ r | s <- stars, r <- inner 0 [s] (delete s stars) ]
  inner cost path [] = [(cost, reverse path)]
  inner cost (s:path) stars =
    [ r | t <- stars, r <- inner (cost + table M.! (s,t)) (t:s:path) (delete t stars) ]

{-
*Main> main
(141,["Arbre","Tambi","Snowdin","Faerun","Straylight","Norrath","AlphaCentauri","Tristram"])
part2
(736,["Faerun","Norrath","Tambi","Straylight","Snowdin","Tristram","Arbre","AlphaCentauri"])
-}
