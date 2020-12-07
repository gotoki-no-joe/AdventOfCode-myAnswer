import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.List
-- import Debug.Trace

{-
正直、七面倒くさい探索問題だ。

現在の座標と、ここまでの経路文字列を状態として持つ。
座標が3,3なのがあればそれが成功。
そうでないとき、座標とMD5の両面から、進める新たな状態を生成する
今回、経路が意味を持つので、座標だけで統合したりはしない。
-}

type State = ((Int,Int),BS8.ByteString)

search1 sts
  | null sts = error "no chance"
  | any goaled sts = BS8.unpack $ snd $ head $ filter goaled sts
  | True = search1 (concatMap step sts)

goaled ((3,3),_) = True
goaled _ = False

isOK c = 'b' <= c && c <= 'f'
dxyc = [(0,-1,'U'),(0,1,'D'),(-1,0,'L'),(1,0,'R')]

step ((x,y),bs) =
  [ ((x1,y1),BS8.snoc bs c)
  | ((dx,dy,c),b) <- zip dxyc $ map isOK $ show $ md5 bs
  , b
  , let x1 = x + dx, 0 <= x1, x1 <= 3
  , let y1 = y + dy, 0 <= y1, y1 <= 3
  ]

phase1 passcode = drop (length passcode) $ search1 [((0,0),BS8.pack passcode)]

test1 = phase1 "hijkl"
test2 = map phase1 ["ihgpwlah", "kglvqrro", "ulqzkmiv"]
ans1 = phase1 "hhhxzeay"

{-
書いてみたらそう面倒でもなかった。
そして今度は最短でなくて全探索しろということね。
-}

search2 [] = []
search2 sts = s1s ++ search2 (concatMap step s2s)
  where
    (s1s,s2s) = partition goaled sts

phase2 passcode = subtract (length passcode) $ fromIntegral $ BS8.length $ snd $ last $ search2 [((0,0),BS8.pack passcode)]

test3 = map phase2 ["ihgpwlah","kglvqrro","ulqzkmiv"]
ans2 = phase2 "hhhxzeay"

{-
*Main> test1
"*** Exception: no chance
CallStack (from HasCallStack):
  error, called at ver1.hs:20:16 in main:Main
*Main> test2
["DDRRRD","DDUDRLRRUDRD","DRURDRUDDLLDLUURRDULRLDUUDDDRR"]
*Main> ans1
"DDRUDLRRRD"
*Main> test3
[370,492,830]
*Main> ans2
398
-}
