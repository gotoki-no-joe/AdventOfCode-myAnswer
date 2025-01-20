-- 2025/1/14 spoilerのためにやり直し

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Control.Parallel.Strategies

import Data.Digest.Pure.MD5
import Data.List.Split
import Data.List

import Data.Char

import Debug.Trace

myinput = "abbhdwsy"

{-
ghci> md5 $ BS.pack "abc3231929"
00000155f8105dff7f56ee10fa9b9abd
-}

part1 :: String -> String
part1 pfx =
    take 8 $ map (!! 5) $ filter (all ('0' ==) . take 5) $ map (show . md5 . BSL.fromStrict . BS.pack . (pfx ++) . show) [0 ..]

part1p :: String -> String
part1p pfx =
    take 8 $ map (!! 5) $ concatMap (filter p . runEval . parList rpar . map f) (chunksOf 5 [0 ..])
  where
    md5s = runEval $ parList rpar [0 ..]
    f = show . md5 . BSL.fromStrict . BS.pack . (pfx ++) . show
    p = isPrefixOf "00000"

-- これだと無限リストに parList が走って困ったことになる。
-- 5個ずつのchunkにして、一つ終わったらまた次、みたいな仕組みが必要？

part2 :: String -> String
part2 pfx =
    head $ dropWhile incomplete $ scanl update "........" $
    map (show . md5 . BSL.fromStrict . BS.pack . (pfx ++) . show) [0 ..]

update res digest
  | any ('0' /=) $ take 5 digest     = res
  | notElem (digest !! 5) ['0'..'7'] = res
  | res !! idx /= '.'                = res
  | otherwise                        = traceShowId $ as ++ (digest !! 6) : bs
  where
    idx = digitToInt (digest !! 5)
    (as,_:bs) = splitAt idx res

incomplete = elem '.'

{-
ghci> part2 "abc"
"05ace8e3"
(115.91 secs, 300,325,322,064 bytes)
ghci> part2 myinput
"424a0197"
(161.45 secs, 541,223,776,752 bytes)

ghci> part2 "abc"
".5......"
".5..e..."
".5..e..3"
".5.ce..3"
"05.ce..3"
"05.ce.e3"
"05.ce8e3"
"05ace8e3"
(98.05 secs, 300,325,387,040 bytes)
ghci> part2 myinput
"4......."
"42......"
"42...1.."
"42...19."
"42...197"
"424..197"
"424.0197"
"424a0197"
(159.67 secs, 541,223,841,200 bytes)
ghci>
-}
