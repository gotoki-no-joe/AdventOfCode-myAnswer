import qualified Data.Set as S
import System.IO.Unsafe
import Debug.Trace

type Food = (S.Set String, S.Set String)

parseLine :: String -> Food
parseLine l = (S.fromList $ words as, S.fromList $ map init $ tail $ words bs)
  where
    (as,bs) = span ('(' /=) l

sample = unsafePerformIO $ readFile "sample.txt"
mydata = unsafePerformIO $ readFile "input.txt"

{-
foodsから始め、foodsを総当たりでintersectして続ける
accの中の両方ともsingletonのもの全てについて、L=Rという対応を返す
accとfoodsの両方からL,Rを除く、accの方だけでいいか。
いずれかがemptyになった要素は取り除く
set of setにしないと、重複がえらいことになるがどうしよう。
accを空リストから始め、foodsの先頭要素から一つずつ、
acc + food * acc + food を作り、それを選別する形にすれば、同じものを二度重ねないから大丈夫かしら。
-}

comp1 foods = comp1loop [] S.empty S.empty foods

comp1loop acc is js [] = comp1loop2 acc is js
comp1loop acc is js ((l0,r0):fs) = {- trace msg -} comp1loop acc2 (S.union is aset) (S.union js bset) fs
  where
    f1@(l,r) = (S.difference l0 is, S.difference r0 js)
    new = [(l1,r1) | (al,ar) <- f1:acc, let l1 = S.intersection l al, let r1 = S.intersection r ar, not $ S.null l1, not $ S.null r1]
    acc1 = new ++ acc
    (as,bs) = unzip $ filter (\f -> S.size (fst f) == 1 && S.size (snd f) == 1) acc1 -- found
    aset = S.fromList $ concatMap S.elems as
    bset = S.fromList $ concatMap S.elems bs
    acc2 = [(l1,r1) | (l,r) <- acc1, let l1 = S.difference l aset, let r1 = S.difference r bset, not $ S.null l1, not $ S.null r1]
    msg = "acc\n" ++ show acc ++ "\nis\n" ++ show is ++ "\njs\n" ++ show js ++ "\nl0\n" ++ show l0 ++ "\nr0\n" ++ show r0

comp1loop2 acc is js
  | null as = (acc,is,js)
  | True = comp1loop2 acc2 (S.union is aset) (S.union js bset)
  where
    (as,bs) = unzip $ filter (\f -> S.size (fst f) == 1 && S.size (snd f) == 1) acc
    aset = S.fromList $ concatMap S.elems as
    bset = S.fromList $ concatMap S.elems bs
    acc2 = [(l1,r1) | (l,r) <- acc, let l1 = S.difference l aset, let r1 = S.difference r bset, not $ S.null l1, not $ S.null r1]

{-
step 1
*Main> comp1 $ map parseLine $ lines sample
([],fromList ["fvjkl","mxmxvkd","sqjhc"],fromList ["dairy","fish","soy"])
*Main> comp1 $ map parseLine $ lines mydata
([],fromList ["fvk","hcbdb","jgtb","kjf","lbmt","mmcpg","rhvbn","zrb"],fromList ["dairy","eggs","fish","nuts","sesame","shellfish","soy","wheat"])

導ける対応を全て導いた。

そういうのがない食品の出現回数を求めたいので、
-}

comp2 foods = sum $ map (S.size . flip S.difference is . fst) foods
  where
    ([],is,js) = comp1 foods
