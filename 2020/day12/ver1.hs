sample = "F10\nN3\nF7\nR90\nF11"

type Cmd = (Char,Int)

parse1 :: String -> Cmd
parse1 (c:cs) = (c,read cs)

type State = ((Int,Int),(Int,Int)) -- x,y,dx,dy

initial :: State
initial = ((0,0),(1,0))

step :: Cmd -> State -> State
step ('N',n) ((x,y),d) = ((x,y+n),d)
step ('S',n) ((x,y),d) = ((x,y-n),d)
step ('E',n) ((x,y),d) = ((x+n,y),d)
step ('W',n) ((x,y),d) = ((x-n,y),d)
step ('F',n) ((x,y),d@(dx,dy)) = ((x+n*dx,y+n*dy),d)
step ('R',n) (p,d) = (p,turnR n d)
step ('L',n) (p,d) = (p,turnL n d)

turnR 0 d = d
turnR n (dx,dy) = turnR (n-90) (dy,-dx)
turnL 0 d = d
turnL n (dx,dy) = turnL (n-90) (-dy,dx)

comp1 str = foldl (flip step) initial $ map parse1 $ lines str

dist ((x,y),_) = abs x + abs y

test1 = comp1 sample

ans1 = readFile "input.txt" >>= print . dist . comp1

-- State :: ship x,y, waypoint x,y
initial2 = ((0,0),(10,1))

step2 :: Cmd -> State -> State
step2 ('N',n) (p,(x,y)) = (p,(x,y+n))
step2 ('S',n) (p,(x,y)) = (p,(x,y-n))
step2 ('E',n) (p,(x,y)) = (p,(x+n,y))
step2 ('W',n) (p,(x,y)) = (p,(x-n,y))
step2 ('F',n) ((x,y),d@(dx,dy)) = ((x+n*dx,y+n*dy),d)
step2 ('R',n) (p,d) = (p,turnR n d)
step2 ('L',n) (p,d) = (p,turnL n d)

comp2 str = foldl (flip step2) initial2 $ map parse1 $ lines str

test2 = comp2 sample

ans2 = readFile "input.txt" >>= print . dist . comp2

{-
*Main> test1
((17,-8),(0,-1))
*Main> ans1
2270
*Main> test2
((214,-72),(4,-10))
*Main> ans2
138669
-}
