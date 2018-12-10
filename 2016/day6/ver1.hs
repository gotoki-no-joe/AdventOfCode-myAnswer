import Data.List

exec fn = do
  fi <- readFile fn
  let ls = lines fi
  let ans1 = compute1 ls
  print ans1

compute1 :: [String] -> String
compute1 css = map process $ transpose css where
  process = snd.maximum.map (\s->(length s, head s)).group.sort

test = exec "inp0.txt"

main = exec "input.txt"
