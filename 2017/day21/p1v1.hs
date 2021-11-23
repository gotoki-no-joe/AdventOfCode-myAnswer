{-
ルールは、2x2が6行、残り全てが3x3のもの。
回転反転全ての8通りをキーに、同じ実体を登録したMapを作る。
どちらも[Bool]とする。BitListで整数に写像すれば、IntMapでいけるけど。面倒なのでやめておく。
-}

import qualified Data.Map as M
import Data.List.Split
import Data.List

main1 = do
  co <- readFile "input.txt"
  print $ doPart1 5 co

main2 = do
  co <- readFile "input.txt"
  print $ doPart1 18 co

doPart1X n xs = length $ filter ('#' ==) $ concat (pats !! n)
  where
    m = parse $ lines xs
    pats = pat0 : gen 3 pat0
    gen k pat = pat1 : gen (5-k) pat1
      where
        pat1 = join $ map (map (m M.!)) $ dividek k pat

doPart1 n xs = length $ filter ('#' ==) $ concat (pats !! n)
  where
    m = parse $ lines xs
    pats = pat0 : gen pat0
    gen pat = pat1 : gen pat1
      where
        pat1 = join $ map (map (m M.!)) $ dividek (if even (length pat) then 2 else 3) pat

type Square a = [[a]]

parse :: [String] -> M.Map [Char] (Square Char)
parse ls = M.fromList [(concat p,r) | (l,r) <- map parseLine ls, p <- rotEm l]
  where
    parseLine l = (wordsBy ('/' ==) l1, wordsBy ('/' ==) (drop 4 l2))
      where
        (l1,l2) = span (' ' /=) l

-- 8通りのバリエーションを作る
rotEm :: Square a -> [Square a]
rotEm ass = take 4 bsss ++ take 4 csss
  where
    bsss = ass : map (map reverse) csss
    csss = map transpose bsss

{-
*Main> rotEm [[1,2,3],[4,5,6],[7,8,9]]
[[[1,2,3],[4,5,6],[7,8,9]]
,[[7,4,1],[8,5,2],[9,6,3]]
,[[9,8,7],[6,5,4],[3,2,1]]
,[[3,6,9],[2,5,8],[1,4,7]]
,[[1,4,7],[2,5,8],[3,6,9]]
,[[7,8,9],[4,5,6],[1,2,3]]
,[[9,6,3],[8,5,2],[7,4,1]]
,[[3,2,1],[6,5,4],[9,8,7]]]
-}

-- kn*knのパターンを、n*n個のk*kのパターンに分解するついでに連結する
dividek :: Int -> Square a -> Square [a]
dividek k ass =
  [ [ concat akk
    | akk <- transpose $ map (chunksOf k) ask
    ]
  | ask <- chunksOf k ass
  ]

pat0 = [".#.","..#","###"]

{-
*Main> dividek 3 pat0
[[".#...####"]]
*Main> dividek 2 ["123456","abcdef","ABCDEF","qwerty","uiop90","zxcvbn"]
[["12ab","34cd","56ef"],["ABqw","CDer","EFty"],["uizx","opcv","90bn"]]
*Main> dividek 3 ["123456","abcdef","ABCDEF","qwerty","uiop90","zxcvbn"]
[["123abcABC","456defDEF"],["qweuiozxc","rtyp90vbn"]]
-}

-- n*n個のk*kのパターンをkn*knのパターンに連結する、dividekの逆っぽいもの
join :: Square (Square a) -> Square a
join asss =
  [ concat as
  | ass <- asss, as <- transpose ass
  ]

{-
*Main> join [[["ab","12"],["cd","34"]],[["ef","56"],["gh","78"]]]
["abcd","1234","efgh","5678"]

*Main> main1
136

That's not the right answer; your answer is too low. Curiously, it's the 
right answer for someone else; you might be logged in to the wrong account 
or just unlucky. In any case, you need to be using your puzzle input.

あれま？

*Main> main1
308

That's not the right answer; your answer is too high.

えええ？
-}

sample = "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#\n"

{-
*Main> doPart1 2 sample
12
あってるやん。

3,2,3,2と繰り返すのでなくて、「2で割り切れるなら2、さもなくば（3で割り切れるから）3」なので、6の倍数のときは2で割るのだ。

*Main> main1
176

やれやれ。

*Main> main2
2368161

それだけ？
-}
