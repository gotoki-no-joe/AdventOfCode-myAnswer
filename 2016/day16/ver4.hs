{-
2025/1/18 spoilerのためにやりなおし

以前のコードを再チェック

ver1 : 問題文に忠実に従った
ghci> test1
"01100"
(0.01 secs, 79,280 bytes)
ghci> ans1
"10011010010010010"
(0.01 secs, 223,896 bytes)
ghci> ans2
"10101011110100011"
(42.19 secs, 17,179,978,872 bytes)

ゆっくり数字が出てくるのが面白かった。こんな桁数でも処理できてしまうのが計算機の進化か。
head . dropwhile . iterate
は
until
でできるかな、というくらい。長さをいちいち数えないように横にメモしておくとか、それなりに配慮している。

ver2 : ドラゴンカーブの性質を利用して、長さを指定されずに無限の長さのそれを出力するように改変。
どういうことかというと、
a0 = a
a1 = a0 ++ '0' : (map inv . rev a0)
a2 = a1                             ++ '0' : (map inv . rev a1)
a3 = a2                                                         ++ '0' : (map inv . rev a2)
...
なので、ドラゴンカーブは全てprefixになっている。
これまでに出力したものを01も順序も逆にして保存したものを続ける、という振る舞いを
やり続けることで、好きなだけ長いドラゴンカーブが出力できるということ。
その「今まで出力したもの」も全て同じprefixな内容なので個別に保存せず、
「本物の最終版のドラゴンカーブ」の無限リストの前何文字が自分の取り分か、だけ持っている。いる。

streamのloop1とloop2の二つはいらんよね、というくらい。

ghci> test2
"01100"
(0.01 secs, 87,344 bytes)
ghci> ans1
"10011010010010010"
(0.00 secs, 278,952 bytes)
ghci> ans2
"10101011110100011"
(71.42 secs, 24,333,200,128 bytes)

なんと、こっちのが遅い。
ていうか、それにしてもよくわからないね、と読み直して、
1文字ずつ送ることで長さを管理する必要ないよねこれ、と。
stream2にしたらかなり速くなったので、これでやり直してみる？

ghci> test2
"01100"
(0.01 secs, 83,672 bytes)
ghci> ans1
"10011010010010010"
(0.01 secs, 255,864 bytes)
ghci> ans2
"10101011110100011"
(52.73 secs, 21,558,824,280 bytes)
まだ負けてる。むぅ。

ver3 : 前半はver2のアイデアの劣化コピー。
後半は、多段を一度にする、というアイデアを出してはいるが実装はしていない。

ghci> test2
"01100"
(0.00 secs, 85,768 bytes)
ghci> ans1
"10011010010010010"
(0.00 secs, 261,224 bytes)
ghci> ans2
"10101011110100011"
(93.66 secs, 21,273,616,696 bytes)

なので結果が劣るのもむべなるかな。

-----

再帰関数の例題としてのドラゴンカーブは、メモリに書き留めることはせず、
forward/reverse | buffer/invert の2ビットの引数でひたすら再帰するだけの構造でよく作られる。

そのやり方でやってみるか、あるいは lazy array を使ってできるか？

-}

import qualified Test.QuickCheck as QC

import Data.Array.Unboxed
import Data.Bits
import Data.List.Split
import Control.DeepSeq
import Data.Char
import Data.Word

import Data.Array.ST
import Control.Monad
import Control.Monad.ST

import Debug.Trace

import Data.List
import Data.Bits

-- from ver2
stream xs = result
  where
    result = loop1 0 xs
    loop1 c (x:xs) = x : loop1 (succ c) xs
    loop1 c [] = 0 : loop2 (succ c) (map (1 -) $ reverse $ take c result)
    loop2 c (y:ys) = y : loop2 (succ c) ys
    loop2 c [] = 0 : loop2 (succ c) (map (1 -) $ reverse $ take c result)

stream1 xs = result
  where
    result = loop 0 xs
    loop c (x:xs) = x : loop (succ c) xs
    loop c [] = 0 : loop (succ c) (map (1 -) $ reverse $ take c result)

stream2 xs = result
  where
    result = xs ++ loop (length xs)
    loop len = 0 : map (1 -) (reverse $ take len result) ++ loop (len + len + 1)

prop_stream = do
  k <- QC.chooseInt (2, 100)
  l <- QC.chooseInt (10000,100000)
  xs <- QC.vectorOf k (QC.chooseInt (0, 1))
  return $ take l (stream xs) == take l (stream1 xs)

{-
指定の長さのドラゴンカーブを生成するもう一つのやり方

遅延配列を使う。サイズと初期列を与える。
初期列の長さ以内のとき、そのまま入れる。
そうでないとき、一つ手前の列の長さが判っているなら、そこで折り返した内容にできる。
けど、そうでないとできない。
普通の再帰ドラゴンカーブは、何段展開するかを再帰の引数にするから、位置からでないから、こんなにややこしくならないのな。
-}

build :: String -> Int -> [Bool]
build ols siz = elems arr
  where
    len0 = length ols
    arr :: Array Int Bool
    arr = listArray (1, siz) $
          [c == '1' | c <- ols] ++
          loop len0
    loop len = False : [not $ arr ! i | i <- [len, pred len .. 1]] ++ loop (len + succ len)

{-
後半、パート1に関しては2→1を4回やればいい。
trailing 0 を数えてその回数。

pred $ popCount $ siz .^. pred siz

パート2に関して、21回行う必要がある。
たった21回なのでやれば終わりだけど、高速化するのに、倍々でやると奇数回なのでちょっと迷惑。
3回分、8ビットの全ての場合について表を作成して、それで処理することを7回やる、で速くなるだろうか？
-}

checksumstep :: [Bool] -> [Bool]
checksumstep (x:y:xs) = (x == y) : checksumstep xs
checksumstep [] = []

checksum len bs = fst $ until (odd . snd) (\(bs,l) -> (checksumstep bs, div l 2)) (bs, len)

-- 短い版
short str len = decode $ checksum len $ build str len

test1 = short "10000" 20

decode xs = [if b then '1' else '0' | b <- xs]

part1 = short "00111101111101000" 272

{-
長い版
-}

checksum3table :: Array Int Bool
checksum3table = listArray (0, 255) $ map f [0 .. 255]
  where
    f :: Int -> Bool
    f i = head $ checksumstep $ checksumstep $ checksumstep [testBit i p | p <- [0 .. 7]]

longchecksum len bs = fst $ until (odd . snd) (\(bs, l) -> (apply3 bs, div l 8)) (bs, len)
  where
    apply3 bs = force [checksum3table ! sum [bit i | (i, True) <- zip [0 ..] b8] | b8 <- chunksOf 8 bs]

long str len = decode $ longchecksum len $ build str len

part2 = long "00111101111101000" 35651584

{-
ghci> test1
"01100"
(0.01 secs, 85,248 bytes)
ghci> part1
"10011010010010010"
(0.00 secs, 255,488 bytes)
ghci> part2
"10101011110100011"
(85.38 secs, 30,234,600,528 bytes)

遅くなってるやん！！！

ghci> part2
"10101011110100011"
(69.00 secs, 29,631,963,440 bytes)

Control.DeepSeq.force を入れたらマシにはなったが。ver1の42secって何だったのさ。
-}

part2s = short "00111101111101000" 35651584

{-
ghci> part2s
"10101011110100011"
(69.11 secs, 20,820,909,576 bytes)

ショートカットなしでも変化なし？

全て配列でやって何とかしよう。
-}

buildA :: String -> Int -> UArray Int Bool
buildA ols siz = listArray (1, siz) $ elems arr
  where
    len0 = length ols
    arr :: Array Int Bool
    arr = listArray (1, siz) $
          [c == '1' | c <- ols] ++
          loop len0
    loop len = False : [not $ arr ! i | i <- [len, pred len .. 1]] ++ loop (len + succ len)

-- checksumは、自分で再起動かかる形に

checksumA :: UArray Int Bool -> UArray Int Bool
checksumA arr
  | mod siz 8 == 0 = checksumA longmode -- 3bit mode
  | even siz = checksumA shortmode -- 1bit mode
  | otherwise = arr -- ende
  where
    (_,siz) = bounds arr
    shortmode = listArray (1, div siz 2) [arr ! i == arr ! succ i | i <- [1,3 .. siz]]
    longmode  = listArray (1, div siz 8) [checksum3table ! sum [bit j | j <- [0 .. 7], arr ! (i+j)] | i <- [1, 9 .. siz]]

outputA :: IArray a Bool => a Int Bool -> String
outputA arr = map ollo $ elems arr
ollo False = '0'
ollo True  = '1'

compute xs siz = outputA $ checksumA $ buildA xs siz

part1arr = compute "00111101111101000" 272
part2arr = compute "00111101111101000" 35651584

{-
ghci> part1arr
"10011010010010010"
(0.01 secs, 550,232 bytes)
ghci> part2arr
"10101011110100011"
(78.52 secs, 46,097,823,496 bytes)
-}

stream5 :: Int -> [Int] -> [Int]
stream5 sz xs = take sz result
  where
    result = xs ++ loop (length xs)
    loop len = 0 : foldl (\acc c -> (1 - c) : acc) (loop (len + len + 1)) (take len result)

stream5a :: Int -> [Int] -> [Int]
stream5a sz xs = take sz result
  where
    dragon xs = xs ++ 0 : map (1 -) (reverse xs)
    (result, _) = until ((sz <=) . snd) (\(xs,l) -> (dragon xs, l + succ l)) (xs, length xs)

stream5b :: Int -> [Int] -> [Int]
stream5b sz xs = drop (sz1 - sz) result
  where
    dragon xs = map (1 -) (reverse xs) ++ 0 : xs
    (result, sz1) = until ((sz <=) . snd) (\(xs,l) -> (dragon xs, l + succ l)) (reverse xs, length xs)

stream5c :: Int -> [Int] -> [Int]
stream5c sz xs = loop (reverse xs) (length xs)
  where
    loop xs len
      | len >= sz = drop (len - sz) xs
--      | otherwise = loop (foldl' (\acc x -> (1 - x) : acc) (0 : xs) xs) (len + succ len) -- 61sec
      | otherwise = loop (map (1 -) (reverse xs) ++ 0 : xs) (len + succ len) --49sec

checksum5 :: Int -> [Int] -> [Int]
checksum5 sz xs
  | odd sz = xs
--  | mod sz 8 == 0 = checksum5 (div sz 8) (bigloop xs) -- なくても1秒しか遅くならない！
  | otherwise = checksum5 (div sz 2) (loop xs)
  where
    loop (x:y:xs) = (if x == y then 1 else 0) : loop xs
    loop _ = []
    bigloop [] = []
    bigloop xs = (if checksum3table ! foldl (\acc i -> i + acc + acc) 0 as then 1 else 0) : bigloop bs
      where
        (as,bs) = splitAt 8 xs

compute5 xs sz = map intToDigit $ reverse $ checksum5 sz $ stream5c sz $ map digitToInt xs

part15 = compute5 "00111101111101000" 272
part25 = compute5 "00111101111101000" 35651584

-- どうやってもver1を越えない。

imperative :: String -> Int -> String
imperative xs sz = runST $
  do
    arr <- newArray_ (0, pred sz) :: ST s (STUArray s Int Bool)
-- 初期列書き込み
    forM_ (zip [0..] xs) (\(i, c) -> writeArray arr i ('1' == c))
-- 折り返し展開
    loop <- fixST $ \loop -> return $ \i j ->
      case () of
        _ | i == sz   -> return ()
          | j == -1   -> writeArray arr i False >> loop (succ i) (pred i)
          | otherwise -> readArray arr j >>= writeArray arr i . not >> loop (succ i) (pred j)
    loop (length xs) (-1)
--    getElems arr >>= traceShowM . map ollo
-- チェックサム縮小
    loop2 <- fixST $ \loop2 -> return $ \len ->
      case () of
        _ | odd len -> return len
          | mod len 8 == 0 -> do
              let len8 = div len 8
              forM_ [0 .. pred len8] (\i -> do
                bs <- forM [i * 8 .. pred $ succ i * 8] (readArray arr)
                let [a,b,c,d,e,f,g,h] = bs
                writeArray arr i (checksum3table ! (a,b,c,d,e,f,g,h))
                )
              loop2 len8
          | otherwise -> do
              let len2 = div len 2
              forM_ [0 .. pred len2] (\i -> do
                a <- readArray arr (i + i)
                b <- readArray arr (i + succ i)
                writeArray arr i (a == b)
                )
              loop2 len2
    len <- loop2 sz
    forM [0 .. pred len] (\i -> do
      b <- readArray arr i
      return $ if b then '1' else '0'
      )
  where
    bnds = ((False,False,False,False,False,False,False,False),(True,True,True,True,True,True,True,True))
    checksum3table :: Array (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool) Bool
    checksum3table = listArray bnds $ map f $ range bnds
    f (a,b,c,d,e,f,g,h) = head $ checksumstep $ checksumstep $ checksumstep [a,b,c,d,e,f,g,h]

part1i = imperative "00111101111101000" 272
part2i = imperative "00111101111101000" 35651584

{-
ghci> part1i
"10011010010010010"
(0.03 secs, 1,218,624 bytes)
ghci> part2i
"10101011110100011"
(203.41 secs, 151,737,657,960 bytes)
インタプリタだと遅いよねSTArrayは。

ghci> part2i
"10101011110100011"
(151.26 secs, 115,230,643,600 bytes)
8ビットショートカットで大幅な高速化を達成したぞ（棒）
-}

data Dragon
  = Str [Bool]
  | Triple Dragon Bool Dragon
  | Rev Dragon

buildD sz xs = get dragon []
  where
    lenxs = length xs
    (dragon,_) = until ((sz <=) . snd)
                   (\(d,l) -> (Triple d False (Rev d), l + succ l))
                   (Str [x == '1' | x <- xs], lenxs)
    get (Str bs) rest = bs ++ rest
    get (Triple a b c) rest = get a $ b : get c rest
    get (Rev (Rev d)) rest = get d rest
    get (Rev (Str bs)) rest = map not (reverse bs) ++ rest
    get (Rev (Triple a b c)) rest = get (Rev c) $ not b : get (Rev a) rest

checksumD sz bs
  | sz .&. 7 == 0 = checksumD (sz .>>. 3) (bigstep bs)
  | even sz = checksumD (div sz 2) (step bs)
  | otherwise = take sz bs
  where
    step (x:y:xs) = (x == y) : step xs
    bigstep xs = (checksum3table ! sum [b | (b,True) <- zip (map bit [0 .. 7]) xs]) : bigstep (drop 8 xs)

compute6 xs sz = decode $ checksumD sz $ buildD sz xs

part16 = compute6 "00111101111101000" 272
part26 = compute6 "00111101111101000" 35651584

{-
ghci> part16
"10011010010010010"
(0.00 secs, 199,624 bytes)
ghci> part26
"10101011110100011"
(29.19 secs, 12,739,617,736 bytes)
ようやくできた！
-}
