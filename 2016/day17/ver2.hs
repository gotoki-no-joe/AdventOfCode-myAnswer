-- 2025-1-20 for spoiler

import Data.List
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as BSL

findPaths passcode = unfoldr step [((0,0), BSL.pack passcode)]
  where
    doors (i,j) = [i > 0, i < 3, j > 0, j < 3] -- UDLRに行けるか
    udlr (i,j) = [(pred i,j), (succ i, j), (i, pred j), (i, succ j)]

    step ijbs | null ijbs = Nothing
    step ijbs = Just (map (BSL.drop (fromIntegral $ length passcode) . snd) goaled, ijbs1)
      where
        (goaled, conts) = partition (((3,3) ==) . fst) ijbs
        ijbs1 =
          [ (ij1, BSL.snoc bs d)
          | (ij, bs) <- conts
          , let passcheck = map (\c -> 'b' <= c && c <= 'f') $ take 4 $ show $ md5 bs
          , (True, True, ij1, d) <- zip4 (doors ij) passcheck (udlr ij) "UDLR"
          ]

input = "hhhxzeay"

part1 = head . dropWhile null . findPaths

part2 pc = [(p, BSL.length p) | p <- ps]
  where
    ps = last $ filter (not . null) $ findPaths pc
