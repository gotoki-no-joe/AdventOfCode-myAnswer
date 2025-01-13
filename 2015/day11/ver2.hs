import Debug.Trace

incr ('z':cs) = 'a' : incr cs  -- 繰り上がり
incr ('h':cs) = traceShow 'i' $ 'k' : cs       -- 禁止文字iは飛ばす
incr ('n':cs) = traceShow 'o' $ 'p' : cs       -- 禁止文字oは飛ばす
incr ('k':cs) = traceShow 'l' $ 'm' : cs       -- 禁止文字lは飛ばす
incr ( c :cs) = succ c : cs
incr "" = ""

cond1 cs@(c1:c2:c3:_) = (succ c3 == c2 && succ c2 == c1) || cond1 (tail cs)
cond1 _ = False

cond3 cs =
  case dropWhile not $ zipWith (==) cs $ tail cs of
    True:bs -> or $ drop 1 bs
    _       -> False

next :: String -> String
next = reverse . until cond13 incr . incr . reverse

cond13 xs = cond1 xs && cond3 xs

part1 = next "hxbxwxba"

part2 = next part1
