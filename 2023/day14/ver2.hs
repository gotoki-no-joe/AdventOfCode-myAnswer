import Data.List
import Data.List.Split
import Data.Bits

import Control.Monad
import Control.Monad.ST
import Data.Array.ST

import qualified Data.Map as M

runner i f = do
  ls <- lines <$> readFile i
  print $ f ls

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 ls = sum $ map (score h) $ transpose ls
  where
    h = length ls
    score _ "" = 0
    score s ('#':xs) = score (pred s) xs
    score s ('O':xs) = s + score (pred s) xs
    score s ('.':xs) =
      case (elemIndex 'O' xs, elemIndex '#' xs) of
        (Nothing, _)             -> 0                                -- 動く岩はひとつもない
        (Just p, Just q) | q < p -> score (pred s - q) $ drop q xs   -- 動く岩は次の#より向こうにしかない
        (Just p, _)              -> s + score (pred s) (remAt p xs)  -- 動く

remAt p xs = take p xs ++ '.' : drop (succ p) xs

add (a,b) (c,d) = (a+c,b+d)

s2b :: String -> Integer
s2b = foldl' step 0 . filter ('#' /=)
  where
    step acc 'O' = acc .<<. 1 .|. 1
    step acc '.' = acc .<<. 1

countO = length . filter ('O' ==)

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 ls = runST $
  do
    fld <- newListArray bnds $ concat ls :: ST s (STUArray s (Int,Int) Char)
    ((a, _), b, m) <- loop fld 0 M.empty
    let x = a + mod (10^9 - a) (b - a)
        s = head [s | (_,(c,s)) <- M.assocs m, c == x]
    return s
  where
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
    shiftLine fld dir p0 = loop1 p0
      where
        loop1 p | not (inRange bnds p) = return ()
        loop1 p = do
          cp <- readArray fld p
          if cp /= '.' then loop1 (add p dir) else loop2 p (add p dir)
        loop2 p q | not (inRange bnds q) = return ()
        loop2 p q = do
          cq <- readArray fld q
          case cq of
            '#' -> loop1 (add q dir)
            '.' -> loop2 p (add q dir)
            'O' -> writeArray fld p 'O' >> writeArray fld q '.' >> loop2 (add p dir) (add q dir)

    genKey fld = s2b <$> getElems fld
    score fld = sum . zipWith (*) [h, pred h ..] . map countO . chunksOf w <$> getElems fld

    loop fld cnt m = do
      key <- genKey fld
      if M.member key m then return (m M.! key, cnt, m) else do
        sco <- score fld
        forM_ [1 .. w] (\j -> shiftLine fld  (1,0) (1,j))
        forM_ [1 .. h] (\i -> shiftLine fld (0,1)  (i,1))
        forM_ [1 .. w] (\j -> shiftLine fld (-1,0) (h,j))
        forM_ [1 .. h] (\i -> shiftLine fld (0,-1) (i,w))
        loop fld (succ cnt) (M.insert key (cnt, sco) m)
