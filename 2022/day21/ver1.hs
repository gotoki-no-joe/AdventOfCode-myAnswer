{-
遅延評価で一撃だわ。
-}

import qualified Data.Map as M

parse :: String -> (String, M.Map String Int -> Int)
parse l = (init key, rhs)
  where
    (key:ws) = words l
    [wL,op,wR] = ws
    rhs =
      if length ws == 1
      then const (read $ head ws)
      else (\m -> funof op (m M.! wL) (m M.! wR))

funof "+" = (+)
funof "-" = (-)
funof "*" = (*)
funof "/" = div -- \x y -> let (q,r) = divMod x y in if y == 0 then q else error "rem"
funof _ = error "never"

phase1 fn = do
  lrs <- map parse . lines <$> readFile fn
  let m = M.fromList [(l, r m) | (l,r) <- lrs]
--  print m
  print $ m M.! "root"

test1 = phase1 "test.txt"
main1 = phase1 "input.txt"

{-
Double版
2.32974643455e14
Int版
2 32974643455000
-- 除算は切り捨てでいいんか。
-}
