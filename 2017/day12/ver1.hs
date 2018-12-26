import Data.List.Split
import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Debug.Trace

test = body "test.txt"
main = body "input.txt"

body fn = do
  fi <- readFile fn
  let m = M.fromList $ map parse $ lines fi
  let ans1 = compute1 m 0
  print (S.size ans1)
  let ans2 = compute2 m ans1
--  print ans2
  print (length ans2)

parse :: String -> (Int, S.IntSet)
parse cs0 = (read a, S.fromList $ map read bs) where
  (a, ' ':'<':'-':'>':' ':cs1) = break (' ' ==) cs0
  bs = splitOn ", " cs1

compute1 :: M.IntMap S.IntSet -> Int -> S.IntSet
compute1 m z = loop S.empty (m M.! z) where
  loop s1 s2
    | s1 == s2 = s1
    | otherwise = loop s2 $ S.union s2 $ S.unions $ map (m M.!) $ S.elems s2

compute2 :: M.IntMap S.IntSet -> S.IntSet -> [S.IntSet]
compute2 m g0 = loop (M.keysSet m S.\\ g0) [g0] where
  loop nodes groups
    | S.null nodes = groups
    | otherwise = -- trace (show (S.size nodes)) $
      loop (nodes S.\\ newgroup) (newgroup:groups) where
       newgroup = compute1 m (S.findMin nodes)

{-
*Main> test
6
2
*Main> main
175
213
-}
