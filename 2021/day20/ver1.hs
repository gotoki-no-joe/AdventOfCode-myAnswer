import Data.Array

type Alg = Array Int Bool
type Bitmap = Array (Int,Int) Bool

parse :: String -> (Alg, Bitmap)
parse xs = (alg, img)
  where
    (l1:_:ls) = lines xs
    alg = listArray (0,511) $ map ('#' ==) l1
    w = length (head ls)
    h = length ls
    img = listArray ((1,1),(h,w)) $ map ('#' ==) $ concat ls

{-
*Main> xs <- readFile "sample.txt"
*Main> (alg, img) = parse xs
*Main> alg ! 0
False
*Main> map (alg !) [1,2,4,8,16,32,64,128,256]
[False,True,True,True,False,True,False,True,False]

周囲は無限にFalseだとして、1ビットだけonでもTrueがあるので、これは1ピクセルずつ広がる。
なので、範囲外にアクセスした場合はFalseを返すラッパーをかませて今のbitmapを読んだうえで、
1ピクセルずつ広げた画像を次のステップでは生成する必要がある。
9ピクセルともにFalseの場合はFalseのままなので、いきなり世界が反転することはない。(sampleは。)
-}

step :: Alg -> Bitmap -> Bitmap
step alg bm = array ((yy0, xx0),(yy1, xx1)) [((y,x), f y x) | y <- [yy0..yy1], x <- [xx0..xx1]]
  where
    ((y0,x0),(y1,x1)) = bounds bm
    read y x = y0 <= y && y <= y1 && x0 <= x && x <= x1 && bm ! (y,x)
    (yy0, xx0) = (pred y0, pred x0)
    (yy1, xx1) = (succ y1, succ x1)
    b n y x = if read y x then n else 0
    f y x = alg ! (b 256 y0 x0 + b 128 y0 x + b 64 y0 x1 + b 32 y x0 + b 16 y x + b 8 y x1 + b 4 y1 x0 + b 2 y1 x + b 1 y1 x1)
      where
        (y0, x0) = (pred y, pred x)
        (y1, x1) = (succ y, succ x)

test1 = do
  xs <- readFile "sample.txt"
  let (alg,img) = parse xs
  let img2 = step alg (step alg img)
  let ans = length $ filter id $ elems img2
  print ans

{-
*Main> test1
35
ここまではいい。ところが、

*Main> xs <- readFile "input.txt"
*Main> (alg,img) = parse xs
*Main> alg ! 0
True
*Main> map (alg !) [1,2,4,8,16,32,64,128,256]
[True,True,True,False,False,True,False,True,True]

無限遠までのFalseが、全てTrueにひっくり返るというとんでもないルールになっている。ええっ？
なので、周囲をTrueで考えるevenStepを作ろう。
-}

evenStep :: Alg -> Bitmap -> Bitmap
evenStep alg bm = array ((yy0, xx0),(yy1, xx1)) [((y,x), f y x) | y <- [yy0..yy1], x <- [xx0..xx1]]
  where
    ((y0,x0),(y1,x1)) = bounds bm
    read y x = y0 > y || y > y1 || x0 > x || x > x1 || bm ! (y,x)
    (yy0, xx0) = (pred y0, pred x0)
    (yy1, xx1) = (succ y1, succ x1)
    b n y x = if read y x then n else 0
    f y x = alg ! (b 256 y0 x0 + b 128 y0 x + b 64 y0 x1 + b 32 y x0 + b 16 y x + b 8 y x1 + b 4 y1 x0 + b 2 y1 x + b 1 y1 x1)
      where
        (y0, x0) = (pred y, pred x)
        (y1, x1) = (succ y, succ x)

main1 = do
  xs <- readFile "input.txt"
  let (alg,img) = parse xs
  let img2 = evenStep alg (step alg img)
  let ans = length $ filter id $ elems img2
  print ans

{-
*Main> main1
5786

いや、このevenStepでうまくいくのは、ある意味偶然だな。
alg ! 511 == False で、塗りつぶされた領域は次に真っ白になるから。
まぁそうでないと扱いきれないけどねぇ。
-}

test2 = do
  xs <- readFile "sample.txt"
  let (alg,img) = parse xs
  let img2 = iterate (step alg) img !! 50
  let ans = length $ filter id $ elems img2
  print ans

run2 = do
  xs <- readFile "input.txt"
  let (alg,img) = parse xs
  let img2 = iterate (evenStep alg . step alg) img !! 25
  let ans = length $ filter id $ elems img2
  print ans

sampleimage = do
  xs <- readFile "sample.txt"
  let (alg,img) = parse xs
  let img2 = iterate (step alg) img !! 50
  let ans = length $ filter id $ elems img2
  print ans
  writeFile "sample.out.txt" $ makeimage img2

makeimage :: Bitmap -> String
makeimage bm = unlines [ [if bm ! (y,x) then '#' else ' ' | x <- [x0..x1]] | y <- [y0..y1] ]
  where
    ((y0,x0),(y1,x1)) = bounds bm

quizimage = do
  xs <- readFile "input.txt"
  let (alg,img) = parse xs
  let img2 = iterate (evenStep alg . step alg) img !! 25
  let ans = length $ filter id $ elems img2
  print ans
  writeFile "input.out.txt" $ makeimage img2

{-
*Main> test2
3351
*Main> run2
16757

最終結果を見てみたけど、特に面白くないな。
-}
