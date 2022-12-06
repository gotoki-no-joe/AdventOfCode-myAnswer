main = do
    fi <- readFile "input.txt"
    let ts = map line2triple $ lines fi
    let as = map oneArea ts
    print $ sum as
    let rs = map ribbon ts
    print $ sum rs

{- 'x' で区切られた数字列を読み取る横着なコード -}
line2triple :: String -> (Int,Int,Int)
line2triple cs = (l,w,h) where
    x2s 'x' = ' '
    x2s  c  =  c
    [l,w,h] = map read $ words $ map x2s cs

{-
l,w,hのうち最大のものを除いた、最小と2番目のものの積、を計算するのは面倒なので、
全て掛けてから最大値で割ってキャンセルする。
数があまり大きいとオーバーフローの心配とかあるので、横着なコード。
-}
oneArea :: (Int,Int,Int) -> Int
oneArea (l,w,h) = 2*l*w + 2*h*l + 2*w*h + l*w*h`div`(maximum [l,w,h])

{-
oneArea同様に、最大値を除外する代わりに全部足してから最大値を引く。
-}
ribbon :: (Int,Int,Int) -> Int
ribbon (l,w,h) = (l + w + h - (maximum [l,w,h]))*2 + l*w*h

{-
*Main> main
1598415
3812909

パート1とパート2でやることにあまり変化がなかった。
-}
