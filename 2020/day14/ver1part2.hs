import Data.Bits
import Data.Char
import qualified Data.Map as M

{-
後半は予想通りという感じ。
それほどXの数は多くないので、全部書き込んでしまっても大丈夫かな？
数え上げをするのが面倒なだけで。

0 アドレスそのまま
1 強制1にする 1を or
X 数え上げる

一つの元アドレスに対して、どうやってふらつきを生成しようか。
マスクはそのまま文字列で持つのが今回は正解だろう。
-}

sample2 = "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1"

data Instr = Mask String | Write Int Int deriving Show

parse1 str
  | str !! 1 == 'a' = Mask $ drop 7 str
  | True            = parseWrite str

parseWrite str = Write (read s1) (read s3)
  where
    (s1,s2) = span isDigit (drop 4 str)
    s3 = drop 4 s2

expandAddr ad mask = loop 35 mask
  where
    loop (-1) "" = [ 0 ]
    loop i ('0':mask) = map ((bit i .&. ad) .|.) $ loop (pred i) mask
    loop i ('1':mask) = map (bit i .|.) $ loop (pred i) mask
    loop i ('X':mask) = [f a | a <- loop (pred i) mask, f <- [id, (bit i .|.)]]

exec :: [Instr] -> M.Map Int Int
exec is = snd $ foldl step (undefined, M.empty) is
  where
    step (mask, mem) (Mask mask1) = (mask1,mem)
    step (mask, mem) (Write ad da) = (mask, M.union (M.fromList [(a,da)|a <- expandAddr ad mask]) mem)

comp2 str = sum $ M.elems $ exec $ map parse1 $ lines str

test2 = comp2 sample2

ans2 = readFile "input.txt" >>= print . comp2

{-
*Main> comp2 sample2
208
*Main> ans2
3705162613854
-}
