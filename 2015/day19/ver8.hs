-- 2025/1/13 spoilerのためにやり直し
-- Word8で高速化を試みる

import Data.Char

import qualified Data.IntMap as IM
import Data.Word
import qualified Data.Set as S
import Data.List
import Debug.Trace
import Control.Applicative

encode :: String -> [Int]
encode (c1:c2:cs) | isLower c2 = ord c1 * 256 + ord c2 : encode cs
encode (c:cs) = ord c : encode cs
encode "" = []

parse :: String -> (Int, [Int])
parse xs = (head $ encode w1, encode w3)
  where
    [w1,_,w3] = words xs

runner i f = do
  ls <- lines <$> readFile i
  let n = length ls
      rules = map parse $ take (n - 2) ls
      mol = encode $ last ls
  print $ f rules mol

part1 rules mol = S.size mols
  where
    rs = IM.fromListWith (++) [(l,[r]) | (l,r) <- rules]
    mols = S.fromList [a ++ d ++ c | (a,b:c) <- zip (inits mol) (tails mol), d <- IM.findWithDefault [] b rs]

main1 = runner "input.txt" part1

main2 = runner "input.txt" part2

part2 rules mol0 = iter 0 S.empty [(0, conv mol0)]
  where
-- 原子の16ビットの番号から背番号に写す
    atoms = S.fromList [i | (l,r) <- rules, i <- l:r]
-- conv :: [Int] -> [Word8]
    conv = map (fromIntegral . flip S.findIndex atoms)

-- 逆引き規則表
    revrules = IM.fromListWith (++) [(fromIntegral cr1, [(cl, cr)]) | (l,r1:r) <- rules, let cl:cr1:cr = conv (l:r1:r)]
-- 逆引き実行
    revstep mol =
      [ a ++ l : drop (length r) c
      | (a,b:c) <- zip (inits mol) (tails mol)
      , (l,r) <- IM.findWithDefault [] (fromIntegral b) revrules
      , isPrefixOf r c]

    electron = conv $ encode "e"

    iter :: Int -> S.Set [Word8] -> [(Int,[Word8])] -> [(Int,Int,Int)] -- 枝刈り成功回数も数える
    iter killed dones [] = [(-1, killed, S.size dones)]
    iter killed dones ((cnt,mol):cmols)
      | electron == mol    = (cnt, killed, S.size dones):iter killed dones1 cmols -- 探索総数も報告する
      | S.member mol dones = iter (succ killed) dones cmols
      | otherwise          = iter killed dones1 cmols1
      where
        mol1s = revstep mol
        cnt1 = succ cnt
        dones1 = S.insert mol dones
        cmols1 = foldr step cmols mol1s
        step m r
          | S.member m dones1 = r
          | otherwise = (cnt1, m) : r
