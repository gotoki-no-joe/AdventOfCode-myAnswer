{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

type State = (Q, ([Int],Int,[Int]))

data Q = A | B | C | D | E | F

run = sum as + c + sum bs 
  where
    (_,(as,c,bs)) = runloop 12667664 (A, ([], 0, []))
    runloop 0 st = st
    runloop cnt st = runloop (pred cnt) $ tm st 

right (as,c,[]) = (c:as,0,[])
right (as,c,b:bs) = (c:as,b,bs)

left ([],c,bs) = ([],0,c:bs)
left (a:as,c,bs) = (as,a,c:bs)

tm (A,(as,0,bs)) = (B, right(as,1,bs))
tm (A,(as,1,bs)) = (C, left (as,0,bs))
tm (B,(as,0,bs)) = (A, left (as,1,bs))
tm (B,(as,1,bs)) = (D, right(as,1,bs))
tm (C,(as,0,bs)) = (B, left (as,0,bs))
tm (C,(as,1,bs)) = (E, left (as,0,bs))
tm (D,(as,0,bs)) = (A, right(as,1,bs))
tm (D,(as,1,bs)) = (B, right(as,0,bs))
tm (E,(as,0,bs)) = (F, left (as,1,bs))
tm (E,(as,1,bs)) = (C, left (as,1,bs))
tm (F,(as,0,bs)) = (D, right(as,1,bs))
tm (F,(as,1,bs)) = (A, right(as,1,bs))

{-
*Main> run
4769
ちょっと時間かかる。
-}
