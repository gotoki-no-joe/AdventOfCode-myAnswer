-- 2025/1/17 for spoiler

-- day11 で作った、汎用なBFS関数

import qualified Data.Set as S
import Data.Bits

isOpen :: Int -> (Int, Int) -> Bool
isOpen base (x,y) = even $ popCount $ x * (x + 3 + y + y) + y * succ y + base


bfs :: Ord a => [a] -> (a -> [a]) -> a -> (Either Int Int, Int)
bfs inits gen goal = loop 0 st0 st0
  where
    st0 = S.fromList inits
    loop cnt visited sts
      | S.null sts        = (Left cnt , S.size visited)  -- 行き詰まった
      | S.member goal sts = (Right cnt, S.size visited) -- 到達した
      | otherwise         = loop (succ cnt) visited1 sts1
      where
        sts1 = S.fromList [st1 | st <- S.elems sts, st1 <- gen st, S.notMember st1 visited]
        visited1 = S.union visited sts1

-- 壁でない周囲を、移動可能な場所として伝える
gen :: Int -> (Int,Int) -> [(Int,Int)]
gen base (x0,y0) =
  [ (x,y)
  | (x,y) <- [(pred x0,y0),(succ x0,y0),(x0, pred y0),(x0, succ y0)]
  , x >= 0, y >= 0
  , isOpen base (x,y)]

sample = bfs [(1,1)] (gen 10) (7,4)

main1 = bfs [(1,1)] (gen 1352) (31,39)

-- パート1は、回り込む可能性から、ぎりぎりで考えることはできなかった。
-- パート2は逆に、50ステップではマンハッタン距離50までしか行けないので、
-- そこまでの迷路を先に生成してPAINTでもいい。
-- というか、上のbfsの定義では「50歩で停止」ができないのでは。

bfs1 :: Ord a => [a] -> (a -> [a]) -> Int -> S.Set a
bfs1 inits gen steps = loop steps st0 st0
  where
    st0 = S.fromList inits
    loop cnt visited sts
      | S.null sts = visited  -- 行き詰まった
      | cnt == 0   = visited -- 到達した
      | otherwise  = loop (pred cnt) visited1 sts1
      where
        sts1 = S.fromList [st1 | st <- S.elems sts, st1 <- gen st, S.notMember st1 visited]
        visited1 = S.union visited sts1

main2 = S.size $ bfs1 [(1,1)] (gen 1352) 50
