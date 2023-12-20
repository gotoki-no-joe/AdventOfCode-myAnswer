import Data.List.Split
import qualified Data.Map as M
import qualified Data.IntSet as IS

import Data.Char
import Debug.Trace
import Data.Array

main1 fn body = do
    (wfs:parts:_) <- wordsBy null . lines <$> readFile fn
    print $ body wfs parts

data Part = P { x :: Int, m :: Int, a :: Int, s :: Int } deriving (Read, Show)

part1 wfs parts = sum [x + m + a + s | p@(P x m a s) <- ps, workflow M.! "in" $ p]
  where
    ps = map (read . ("P " ++)) parts :: [Part]
    workflow :: M.Map String (Part -> Bool)
    workflow = M.fromList $ map parseWF wfs
    parseWF :: String -> (String, Part -> Bool)
    parseWF l = (nm, rs)
      where
        (nm, _:l1) = span isAlpha l
        rs = foldr parseWF1 (\p -> error $ "hoge" ++ show p) $ wordsBy (',' ==) $ init l1
    parseWF1 :: String -> (Part -> Bool) -> (Part -> Bool)
    parseWF1 w f =
      case span (':' /=) w of
        (rhs, []) -> parseRHS rhs
        ('x':'>':ns, _:rhs) -> \p -> if x p > read ns then parseRHS rhs p else f p
        ('x':'<':ns, _:rhs) -> \p -> if x p < read ns then parseRHS rhs p else f p
        ('m':'>':ns, _:rhs) -> \p -> if m p > read ns then parseRHS rhs p else f p
        ('m':'<':ns, _:rhs) -> \p -> if m p < read ns then parseRHS rhs p else f p
        ('a':'>':ns, _:rhs) -> \p -> if a p > read ns then parseRHS rhs p else f p
        ('a':'<':ns, _:rhs) -> \p -> if a p < read ns then parseRHS rhs p else f p
        ('s':'>':ns, _:rhs) -> \p -> if s p > read ns then parseRHS rhs p else f p
        ('s':'<':ns, _:rhs) -> \p -> if s p < read ns then parseRHS rhs p else f p
    parseRHS :: String -> Part -> Bool
    parseRHS "R" = const False
    parseRHS "A" = const True
    parseRHS rhs = workflow M.! rhs

{-
コンマで区切るとかでちまちまやるのと、parser書くのとどっちが早いかな？

ghci> data R = R { x :: Int, m :: Int} deriving (Read, Show)
ghci> read "R {x=1,m=2}" :: R
R {x = 1, m = 2}

ghci> main1 "sample.txt" part1
19114
ghci> main1 "input.txt" part1 
323625

パート2、記号実行しろってか？
各ルールは一度しか参照されない、であってるかな？
ループはないだろうけど、合流があってもまぁ関係ないか。

xmasのそれぞれについて、1～4000全体集合を与える。区間で表現するべきかこれ？
条件があるたびに、それを満たすものと満たさないものに2つに分けて、ラベルに飛ぶか次に進むかする。
二分割されるので、複数のAの間に重複がないことは保証される。これはありがたい。
分岐で空虚になったものはその時点で捨てればよい。
Aされたものの要素数を掛け合わせて場合の数を求めて、その総和を取る。

x > n に対して、succ n .. 4000 との intersection と、difference をとる感じでいいかしら。かなり手抜き。
part1では Part -> Bool な関数を作ったけれど、これを
Part -> [(Part, String)] という飛び先の関数にする？ではなくて、
Part2 -> Int という、結果の合計を返す計算を組み上げてしまおう。デバッグがつらいけど。
-}

type Part2 = Array Int IS.IntSet

xmas 'x' = 0
xmas 'm' = 1
xmas 'a' = 2
xmas 's' = 3

part2 wfs parts = workflow M.! "in" $ listArray (0,3) $ replicate 4 (IS.fromDistinctAscList [1..4000])
  where
    ps = map (read . ("P " ++)) parts :: [Part]
    workflow :: M.Map String (Part2 -> Int)
    workflow = M.fromList $ map parseWF wfs
    parseWF :: String -> (String, Part2 -> Int)
    parseWF l = (nm, rs)
      where
        (nm, _:l1) = span isAlpha l
        rs = foldr parseWF1 (\p -> error $ "hoge" ++ show p) $ wordsBy (',' ==) $ init l1
    parseWF1 :: String -> (Part2 -> Int) -> (Part2 -> Int)
    parseWF1 w f =
      case span (':' /=) w of
        (rhs, []) -> parseRHS rhs
        (s:'>':ns, _:rhs) -> sub s rhs [succ $ read ns .. 4000] -- \p2 -> if x p > read ns then parseRHS rhs p2 + f p2
        (s:'<':ns, _:rhs) -> sub s rhs [1 .. pred $ read ns] -- \p2 -> if x p < read ns then parseRHS rhs p2 + f p2
      where
        sub s rhs ds p2 =
          (if IS.null p2T then 0 else parseRHS rhs (p2 // [(i,p2T)])) +
          (if IS.null p2F then 0 else f (p2 // [(i,p2F)]))
          where
            i = xmas s
            dS = IS.fromDistinctAscList ds
            p2T = IS.intersection dS $ p2 ! i
            p2F = IS.difference (p2 ! i) dS

    parseRHS :: String -> Part2 -> Int
    parseRHS "R" = const 0
    parseRHS "A" = product . map IS.size . elems
    parseRHS rhs = workflow M.! rhs

{-
ghci> main1 "sample.txt" part2
167524517877236
なんか大きくなっちゃった。あれー？
区間が >,< でなく >=,<= になってた。

ghci> main1 "sample.txt" part2
167409079868000
ghci> main1 "input.txt" part2 
127447746739409

範囲を上手に表現しなくても、IS.fromDistinctAscList [1..4000] で済む、という手抜きが効いたな。

でもよく考えると、合流なしなら(lb,ub)だけで十分表せたな。
-}
