import Data.List.Split
import Data.Char

{-
JSONパーサを書けと言っているわけではない。
字句レベルで判別可能。
文字列の中外を見分ける必要もない。そこには現れないと明言しているから。
なので、全部読めばいい。
-}

notdigit '-' = False
notdigit c = not $ isDigit c

compute1 :: String -> Int
compute1 str = sum $ map read $ wordsBy notdigit str

samples = ["[1,2,3]", "{\"a\":2,\"b\":4}"
          ,"[[[3]]]", "{\"a\":{\"b\":4},\"c\":-1}"
          ,"{\"a\":[-1,1]}", "[-1,{\"a\":1}]"
          ,"[]", "{}"]

test1 = map compute1 samples

main = do
  fi <- readFile "input.txt"
  let li = head $ lines fi
  let ans1 = compute1 li
  print ans1
  putStrLn "part2"
  let (json,"") = parse li
  let ans2 = compute2 json
  print ans2

{-
*Main> main
111754

part1できた
-}

{-
後半はちゃんと木を作って、redを無視しながら集計、
という手順でする必要がありそうだ。
-}

data JSON = JSONnum Int | JSONstr String | JSONarr [JSON] | JSONobj [(String,JSON)]
  deriving (Eq, Show)

parse :: String -> (JSON,String)
parse ('-':cs) = (JSONnum $ negate $ read ns, cs1) where
  (ns,cs1) = break (not.isDigit) cs
parse (c:cs) | isDigit c = (JSONnum $ read ns, cs1) where
  (ns,cs1) = break (not.isDigit) (c:cs)
parse ('\"':cs) = (JSONstr ss, cs1) where
  (ss,'\"':cs1) = break ('\"' ==) cs
parse ('[':cs) = (JSONarr arr, cs1) where
  (arr,cs1) = parseArr1 cs
parse ('{':cs) = (JSONobj ass, cs1) where
  (ass,cs1) = parseObj1 cs

parseArr1 (']':cs) = ([],cs)
parseArr1 cs = (dat:arr, cs2) where
  (dat,cs1) = parse cs
  (arr,cs2) = parseArr2 cs1

parseArr2 (']':cs) = ([],cs)
parseArr2 (',':cs) = (dat:arr, cs2) where
  (dat,cs1) = parse cs
  (arr,cs2) = parseArr2 cs1

parseObj1 ('}':cs) = ([],cs)
parseObj1 ('\"':cs) = ((ss,dat):ass, cs3) where
  (ss,'\"':':':cs1) = break ('\"' ==) cs
  (dat,cs2) = parse cs1
  (ass,cs3) = parseObj2 cs2

parseObj2 ('}':cs) = ([],cs)
parseObj2 (',':'\"':cs) = ((ss,dat):ass, cs3) where
  (ss,'\"':':':cs1) = break ('\"' ==) cs
  (dat,cs2) = parse cs1
  (ass,cs3) = parseObj2 cs2

compute2 :: JSON -> Int
compute2 (JSONnum n) = n
compute2 (JSONstr _) = 0
compute2 (JSONarr a) = sum (map compute2 a)
compute2 (JSONobj a)
  | any (red.snd) a = 0
  | otherwise = sum (map (compute2.snd) a)

red (JSONstr "red") = True
red _ = False

samples2 = ["[1,2,3]", "[1,{\"c\":\"red\",\"b\":2},3]"
           ,"{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}"
           ,"[1,\"red\",5]"]

test2 = map (compute2.fst.parse) samples2

{-
*Main> test1
[6,6,3,3,0,0,0,0]
*Main> test2
[6,4,0,6]
*Main> main
111754
part2
65402
-}
