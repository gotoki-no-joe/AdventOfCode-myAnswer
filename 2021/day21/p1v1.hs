{-
普通にシミュレーションかけたらいいよね。
-}

type PState = -- player state
  ( Int -- score
  , Int -- position
  )

type State = -- System State
  ( PState -- player 1
  , PState -- player 2
  , Int    -- # dice rolled
  )

initial :: Int -> Int -> State
initial p1 p2 = ((0,p2),(0,p1),0)

win :: State -> Bool
win ((s,_),_,_) = s >= 1000

swap :: State -> State
swap (p1,p2,i) = (p2,p1,i)

dice x =
  case mod x 100 of
    0 -> 100
    r -> r

pos x =
  case mod x 10 of
    0 -> 10
    r -> r

turn :: State -> State
turn ((s,p),p2,i) = ((s+p1, p1),p2,i+3)
  where
    d = dice (i+1) + dice (i+2) + dice (i+3)
    p1 = pos (p + d)

main1 p1 p2 = s * d
  where
    (_,(s,_),d) = head . dropWhile (not . win) . iterate (turn . swap) $ initial p1 p2

test1 = main1 4 8

run1 = main1 1 3

{-
*Main> test1
739785
*Main> run1
897798
-}
