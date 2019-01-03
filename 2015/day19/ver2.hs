{-# Language Strict #-}

import Debug.Trace
import qualified Data.Sequence as Q
import qualified Data.Set as S

main = do
  fi <- readFile "input.txt"
  let ls = lines fi
  let rules = map parse $ take (length ls - 2) ls
  let initial = Q.fromList $ last ls
  let ans1 = compute1 rules initial
  print ans1
  let ans2 = compute2a rules initial
  print ans2

{-
単純に文字の並びと思って探すか、
元素記号とおもわしき構文を突き止めて
1文字でのマッチングに徹するか。

とりあえず手軽な前者で。
開始分子に対して、
あらゆる位置とあらゆる規則で、照合を行う必要がある。
rev方式が手間を稼げるかしら。
-}

type Rule = (Q.Seq Char,Q.Seq Char)

process :: [Rule] -> Q.Seq Char -> S.Set (Q.Seq Char)
process rules molecule = S.fromList
  [ ys Q.>< b Q.>< (Q.drop (Q.length a) zs)
  | i <- [0..Q.length molecule]
  , let (ys,zs) = (Q.zip (Q.inits molecule) (Q.tails molecule)) `Q.index` i
  , (a,b) <- rules
  , isPrefixOf a zs
  ]

isPrefixOf xs ys = Q.length xs <= Q.length ys && and [ Q.index xs i == Q.index ys i | i <- [0..length xs - 1] ]

compute1 rules initial = S.size $ process rules initial

parse cs = (Q.fromList a, Q.fromList b) where
  (a,' ':'=':'>':' ':b) = break (' ' ==) cs

testrule = map parse ["H => HO", "H => OH", "O => HH"]

test1 = compute1 testrule $ Q.fromList "HOH"
test2 = compute1 testrule $ Q.fromList "HOHOHO"
test3 = process [parse "H => OO"] $ Q.fromList "H2O"

{-
後半、eからの導出は初回しかない。
幅優先探索で探しても大変なことになりそう。
規則を見ると必ず長くなるので、規則を逆向きに使って、
最後にeに到達する最短のステップを幅優先探索で探すのが
すぐ行き詰まりそうで有効そうだ。
-}

{-
compute2 rules dest = loop 0 [dest] where
  revrules = map (\(a,b) -> (b,a)) rules
  loop n molecules
    | null molecules = error $ unwords ["empty molecules in", show n, "steps"]
    | elem "e" molecules = n
    | otherwise = -- trace (show molecules) $
        loop (succ n) $ nub $ concatMap (process revrules) molecules
-}

testrule2 = map parse
  ["e => H"
  ,"e => O"
  ,"H => HO"
  ,"H => OH"
  ,"O => HH"]

test4 = compute2a testrule2 $ Q.fromList "HOH"
test5 = compute2a testrule2 $ Q.fromList "HOHOHO"

{-
eにする規則は最後の最後で使うだけでいいのに、
やたらと引っ掛かってしまう。
eからの規則は全体にしかマッチしないとするか、
二つeがある分子を殺して候補を減らすか、ぐらいだな。
-}

compute2a rules dest = loop 0 $ S.singleton dest where
  electron = Q.fromList ""
  revrules = map (\(a,b) -> (b,a)) $ filter ((electron /=).fst) rules
  firstmolecs = S.fromList $ map snd $ filter ((electron ==).fst) rules
  loop n molecules
    | S.null molecules = error $ unwords ["empty molecules in", show n, "steps"]
    | not $ S.null (S.intersection firstmolecs molecules) = succ n
    | otherwise = trace (show $ S.size molecules) $
        loop (succ n) $ S.foldr' S.union S.empty $ S.map (process revrules) molecules

{-
C:\Users\ohkubo\Documents\bitbucket\AdventOfCode-myAnswer\2015\day19>.\ver2
576
1
107
5684
199890
ver2: getMBlocks: VirtualAlloc MEM_COMMIT failed: y[WO t@CｪｬｳｷｬｽﾟAｱﾌｮｹﾅｫﾜｹB

わぁぶっ壊れた。
-}
