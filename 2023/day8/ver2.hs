import qualified Data.Map as M
import qualified Data.Set as S


runner i f = do
  l1:_:ls <- lines <$> readFile i
  let m = M.fromList $ map parse ls
  print $ f l1 m

test1 = runner "samp1.txt" part1
main1 = runner "input.txt" part1

parse :: String -> (String,(String,String))
parse l = (take 3 l, (take 3 $ drop 7 l, take 3 $ drop 12 l))

part1 lrs m = loop 0 "AAA" (cycle lrs)
  where
    loop cnt "ZZZ" _ = cnt
    loop cnt pos (x:xs) = loop (succ cnt) pos1 xs
      where
        pos1 = (if x == 'L' then fst else snd) $ m M.! pos

{-
ghci> test1
2
ghci> main1
12169
-}

check1 lrs m = unlines $ show (length lrs) : map (\p -> loop S.empty 0 p (cycle lrs)) starts
  where
    starts = filter (('A' ==) . last) $ M.keys m
    loop vis cnt pos (x:xs) = disp ++ cont
      where
        isAZ = elem (last pos) "AZ"
        pos1 = (if x == 'L' then fst else snd) $ m M.! pos
        vis1 = if isAZ then S.insert pos vis else vis
        cont = if S.member pos vis then "" else loop vis1 (succ cnt) pos1 xs
        disp = if isAZ then unwords [show cnt, pos, "- "] else ""
