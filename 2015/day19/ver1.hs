{-# Language Strict #-}

import Data.List
import Debug.Trace
-- import Data.Set

main = do
  fi <- readFile "input.txt"
  let ls = lines fi
  let rules = map parse $ take (length ls - 2) ls
  let initial = last ls
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

type Rule = (String,String)

process rules molecule = loop "" molecule where
  loop _ [] = []
  loop sx xs =
    [ recover sx (b ++ drop (length a) xs)
    | (a,b) <- rules
    , isPrefixOf a xs
    ]
    ++ loop (head xs:sx) (tail xs)

recover "" xs = xs
recover (y:ys) xs = recover ys (y:xs)

compute1 rules initial = length $ nub $ process rules initial

parse cs = (a,b) where
  (a,' ':'=':'>':' ':b) = break (' ' ==) cs

testrule = map parse ["H => HO", "H => OH", "O => HH"]

test1 = compute1 testrule "HOH"
test2 = compute1 testrule "HOHOHO"
test3 = process [parse "H => OO"] "H2O"

{-
後半、eからの導出は初回しかない。
幅優先探索で探しても大変なことになりそう。
規則を見ると必ず長くなるので、規則を逆向きに使って、
最後にeに到達する最短のステップを幅優先探索で探すのが
すぐ行き詰まりそうで有効そうだ。
-}

compute2 rules dest = loop 0 [dest] where
  revrules = map (\(a,b) -> (b,a)) rules
  loop n molecules
    | null molecules = error $ unwords ["empty molecules in", show n, "steps"]
    | elem "e" molecules = n
    | otherwise = -- trace (show molecules) $
        loop (succ n) $ nub $ concatMap (process revrules) molecules

testrule2 = map parse
  ["e => H"
  ,"e => O"
  ,"H => HO"
  ,"H => OH"
  ,"O => HH"]

test4 = compute2a testrule2 "HOH"
test5 = compute2a testrule2 "HOHOHO"

{-
eにする規則は最後の最後で使うだけでいいのに、
やたらと引っ掛かってしまう。
eからの規則は全体にしかマッチしないとするか、
二つeがある分子を殺して候補を減らすか、ぐらいだな。
-}

compute2a rules dest = loop 0 [dest] where
  revrules = map (\(a,b) -> (b,a)) $ filter (("e" /=).fst) rules
  firstmolecs = map snd $ filter (("e" ==).fst) rules
  loop n molecules
    | null molecules = error $ unwords ["empty molecules in", show n, "steps"]
    | not $ null (intersect firstmolecs molecules) = succ n
    | otherwise = trace (show $ length molecules) $
        loop (succ n) $ nub $ concatMap (process revrules) molecules

{-
細かい部品で効率化を図ろう。
試験管を集合にするか。
なんかあまり効果がないような気が。
-}
