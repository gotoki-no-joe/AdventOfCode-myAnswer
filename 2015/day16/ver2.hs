-- 2022-11-19

import qualified Data.Map as M
import Data.Maybe

mfcsam =
  [("children:",3),("cats:",7),("samoyeds:",2),("pomeranians:",3),("akitas:",0)
  ,("vizslas:",0),("goldfish:",5),("trees:",3),("cars:",2),("perfumes:",1)]

main1 = do
  co <- readFile "input.txt"
  let aunts = map parse $ lines co
  print $ part1 aunts

part1 aunts = [id | (id, aunt) <- zip [1..] aunts, all (check aunt) mfcsam]

check aunt (k,v) = maybe True (v ==) $ M.lookup k aunt

parse xs = M.fromList $ loop ws
  where
    ws = drop 2 $ words xs
--    loop [] = []
    loop [k,v] = [(k, read v)]
    loop (k:v:ws) = (k, read $ init v) : loop ws

part2 aunts =
  [ id
  | (id, aunt) <- zip [1..] aunts
  , all (check   aunt) mfcsamEQ
  , all (checkLT aunt) mfcsamLT
  , all (checkGT aunt) mfcsamGT]

checkLT aunt (k,v) = maybe True (v >) $ M.lookup k aunt
checkGT aunt (k,v) = maybe True (v <) $ M.lookup k aunt

mfcsamEQ = [("children:",3),("samoyeds:",2),("akitas:",0)
           ,("vizslas:",0),("cars:",2),("perfumes:",1)]
mfcsamLT = [("pomeranians:",3),("goldfish:",5)]
mfcsamGT = [("cats:",7),("trees:",3)]

main2 = readFile "input.txt" >>= print . part2 . map parse . lines
