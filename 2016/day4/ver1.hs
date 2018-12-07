import Data.List.Split
import Data.List
import Data.Char (ord, chr)

main = do
  fi <- readFile "input.txt"
  let ls = lines fi
  let ls1 = map parse ls
  let ls2 = filter realroom ls1
  let ans1 = sum $ map (\(_,id,_) -> id) ls2
  print ans1
  let nms = map realname ls2
  let room = filter ((elem "object").fst) nms
  mapM print room

samples =
  [ "aaaaa-bbb-z-y-x-123[abxyz]"
  , "a-b-c-d-e-f-g-h-987[abcde]"
  , "not-a-real-room-404[oarel]"
  , "totally-real-room-200[decoy]"
  ]

{-
まず、セクタIDとそうでない名前とを分離しないと。
さらにチェックサムも取り出す。
-}

type Data = ([String],Int,String)

parse :: String -> Data
parse cs = (name, read id, checksum) where
  ss = endByOneOf "-[]" cs
  (name,[id,checksum]) = splitAt (length ss - 2) ss

{-
チェックサムを作って判定する

名前を連結し、sort/group/length で頻度を数え、
頻度の降順、文字の昇順で整列し、
先頭5つを取り出して並べる
-}

(|>) = flip ($)

realroom :: Data -> Bool
realroom (name,_,checksum) =
  concat name |> sort |> group |> map (\xs -> (- length xs, head xs)) |>
    sort |> take 5 |> map snd |> (== checksum)

{-
何だこの問題。
ともかく復号するか。
0から25にして、ID足して、26を法にして文字に戻す。

あえてunwordsはしない。
-}

realname :: Data -> ([String],Int)
realname (name,id,_) = (map (map f) name, id) where
  f c = chr $ ord 'a' + mod (ord c - ord 'a' + id) 26

{-
*Main> main
409147
(["northpole","object","storage"],991)
-}
