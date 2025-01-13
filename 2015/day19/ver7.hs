-- 2025/1/13 spoilerのためにやり直し

import Data.Char

import qualified Data.IntMap as IM
--import Data.Word
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

part2o rules mol0 = loop 0 mol0
  where
    revrules = IM.fromListWith (++) [(r1, [lr]) | lr@(l,r1:_) <- rules]
    revstep mol =
      [ a ++ l : drop (length r) c
      | (a,b:c) <- zip (inits mol) (tails mol)
      , (l,_:r) <- IM.findWithDefault [] b revrules
      , isPrefixOf r c]

    loop cnt mol = traceShow (cnt, length mol) $
      case revstep mol of
        [] -> (cnt, mol)
        (mol1:_) -> loop (succ cnt) mol1

main2 = runner "input.txt" part2

-- 幅優先探索は辛い
part2b rules mol0 = loop 0 s0 s0
  where
    revrules = IM.fromListWith (++) [(r1, [lr]) | lr@(l,r1:_) <- rules]
    revstep mol =
      [ a ++ l : drop (length r) c
      | (a,b:c) <- zip (inits mol) (tails mol)
      , (l,_:r) <- IM.findWithDefault [] b revrules
      , isPrefixOf r c]

    electron = encode "e"
    s0 = S.singleton mol0

    loop :: Int -> S.Set [Int] -> S.Set [Int] -> Int
    loop cnt dones mols
      | found       = cnt
      | S.null mols = error $ "Empty " ++ show cnt
      | otherwise   = traceShow (cnt, S.size mols) $ loop (succ cnt) dones1 mols1
      where
        found = S.member electron mols
        mols1 = S.fromList $ concatMap (filter (flip S.notMember dones) . revstep) $ S.elems mols
        dones1 = S.union dones mols1

part2d rules mol0 = iter 0 mol0
  where
    revrules = IM.fromListWith (++) [(r1, [lr]) | lr@(l,r1:_) <- rules]
    revstep mol =
      [ a ++ l : drop (length r) c
      | (a,b:c) <- zip (inits mol) (tails mol)
      , (l,_:r) <- IM.findWithDefault [] b revrules
      , isPrefixOf r c]

    electron = encode "e"

    iter :: Int -> [Int] -> Maybe Int
    iter cnt mol
      | found      = Just cnt
      | null mol1s = Nothing
      | otherwise  = iter cnt1 (last mol1s)
--      | otherwise  = foldr (\m r -> iter cnt1 m <|> r) Nothing mol1s
      where
        found = electron == mol
        mol1s = revstep mol
        cnt1 = succ cnt

-- 観察すると、どうやっても 201 105 で行き詰まるんで、そういうトラップが仕掛けられている感じ？
-- 深さ優先探索しつつ、既知のトラップは放棄するようにしないとなのか。
-- ver6は「一番最後に見つかる書き換え候補」を選び続けたら行ける、というコードだしな。
-- そう書いたら一瞬でOK出たので、ぐぬぬ、だ。

part2 rules mol0 = iter S.empty [(0, mol0)]
  where
    revrules = IM.fromListWith (++) [(r1, [lr]) | lr@(l,r1:_) <- rules]
    revstep mol =
      [ a ++ l : drop (length r) c
      | (a,b:c) <- zip (inits mol) (tails mol)
      , (l,_:r) <- IM.findWithDefault [] b revrules
      , isPrefixOf r c]

    electron = encode "e"

    iter :: S.Set [Int] -> [(Int,[Int])] -> Int
    iter dones ((cnt,mol):cmols)
      | electron == mol    = cnt
      | S.member mol dones = iter dones cmols
      | otherwise          = iter dones1 cmols1
      where
        mol1s = revstep mol
        cnt1 = succ cnt
        dones1 = S.insert mol dones
        cmols1 = foldr step cmols mol1s
        step m r
          | S.member m dones1 = r
          | otherwise = (cnt1, m) : r
