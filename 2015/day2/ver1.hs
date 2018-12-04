main = do
  fi <- readFile "input.txt"
  let ls = lines fi
  let ts = map line2triple ls
  let as = map oneArea ts
  print $ sum as
  let rs = map ribbon ts
  print $ sum rs

line2triple :: String -> (Int,Int,Int)
line2triple cs = (read d1, read d2, read d3) where
  (d1,'x':cs1) = break ('x' ==) cs
  (d2,'x':d3) = break ('x' ==) cs1

oneArea :: (Int,Int,Int) -> Int
oneArea (l,w,h) = 2*l*w + 2*h*l + 2*w*h + l*w*h`div`(maximum [l,w,h])

ribbon :: (Int,Int,Int) -> Int
ribbon (l,w,h) = (l + w + h - (maximum [l,w,h]))*2 + l*w*h

{-
*Main> main
1598415
3812909

あまり変化がなかったですな。
-}
