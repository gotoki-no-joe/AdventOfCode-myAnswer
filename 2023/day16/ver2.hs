import Data.Array
import Data.Graph

runner i f = do
  ls <- lines <$> readFile i
  print $ f ls

test12 = runner "sample.txt" part12
main12 = runner "input.txt" part12

part12 ls = (part1ans, part2ans)
  where
    h = length ls
    w = length $ head ls
    bnds = ((1,1,0),(h,w,3))
    lb = index bnds (1,1,0)
    ub = index bnds (h,w,3)
    g = buildG (lb, ub) $ concat
        [ mkEdge i j c
        | (i, l) <- zip [1 ..] ls
        , (j, c) <- zip [1 ..] l
        ]
    mkEdge i j c =
      case c of
        '.' -> sub [[(1,0,0)], [(-1,0,1)], [(0,1,2)], [(0,-1,3)]]
        '/' -> sub [[(0,-1,3)], [(0,1,2)], [(-1,0,1)], [(1,0,0)]]
        '\\'-> sub [[(0,1,2)], [(0,-1,3)], [(1,0,0)], [(-1,0,1)]]
        '-' -> sub [[(0,1,2),(0,-1,3)], [(0,1,2),(0,-1,3)], [(0,1,2)], [(0,-1,3)]]
        '|' -> sub [[(1,0,0)], [(-1,0,1)], [(1,0,0),(-1,0,1)], [(1,0,0),(-1,0,1)]]
      where
        sub ess = [(index bnds (i,j,d), index bnds (i1,j1,dd))
                  | (d,es) <- zip [0 ..] ess, (dx,dy,dd) <- es
                  , let i1 = i + dx, let j1 = j + dy, inRange bnds (i1,j1,0) ]
    v2ijc = listArray (lb, ub) $ range bnds
    energize arg =
      length $ filter id $ elems $
      accumArray (flip const) False ((1,1),(h,w)) $
      [((i,j),True) | (i,j,_) <- map (v2ijc !) $ reachable g $ index bnds arg]
    part1ans = energize (1, 1, 2)

    part2ans = maximum $ map energize $
      [(i,1,2) | i <- [1 .. h]] ++
      [(i,w,3) | i <- [1 .. h]] ++
      [(1,j,0) | j <- [1 .. w]] ++
      [(h,j,1) | j <- [1 .. w]]

{-
ghci> test1
46
ghci> main1
7199
-}

