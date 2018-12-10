import Data.List

exec fn = do
  fi <- readFile fn
  let ls = lines fi
  let mid = map (map (\s -> (length s, head s)).group.sort) $ transpose ls
  let ans1 = map (snd.maximum) mid
  print ans1
  let ans2 = map (snd.minimum) mid
  print ans2

test = exec "inp0.txt"

main = exec "input.txt"

{-
*Main> test
"easter"
"advent"
*Main> main
"wkbvmikb"
"evakwaga" -- ググったら一人だけ出てきたよ。
-}
