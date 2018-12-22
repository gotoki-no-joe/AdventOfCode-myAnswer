{-
よくわからないけれど総当たりしてみるか？
ひとつめが0から100まで、
次からが0から残りまで、
最後が残り全部、みたいにして、配分を総当たりする。
これで重複ないし漏れもないよな。
-}

data Ingredient = Ingredient { name :: String, capacity :: Int, durability :: Int, flavor :: Int, texture :: Int, carories :: Int } deriving Show

-- どの原材料をどれだけ入れるかのリスト
-- 原材料の残り種類, 小さじの必要量
recipes :: Int -> Int -> [[Int]]
recipes k 0 = [replicate k 0]
recipes 1 q = [[q]]
recipes n q =
  [ v:r
  | v <- [0..q]
  , r <- recipes (pred n) (q-v)
  ]

-- レシピの得点
-- score :: [Ingredient] -> [Int] -> Int
score ings appos = product
  [ max 0 $ sum [ f ing * appo | (ing, appo) <- zip ings appos ]
  | f <- [capacity, durability, flavor, texture]
  ]

test = body1 "test.txt"
main = body1 "input.txt"

body1 fn = do
  fi <- readFile fn
  let ings = map parse $ lines fi
  let rs = recipes (length ings) 100
  let ans1 = maximum [ (score ings r, r) | r <- rs ]
  print ans1
  let ans2 = maximum [ (score ings r, r) | r <- rs, caro ings r == 500 ]
  print ans2

parse :: String -> Ingredient
parse cs = Ingredient (init $ ws !! 0) (read  $init $ ws !! 2) (read $ init $ ws !! 4)
                      (read $ init $ ws !! 6) (read $ init $ ws !! 8) (read $ ws !! 10)
  where ws = words cs

-- レシピのカロリー
-- caro :: [Ingredient] -> [Int] -> Int
caro ings appos = sum [ carories ing * appo | (ing, appo) <- zip ings appos ]

{-
*Main> test
(62842880,[44,56])
(57600000,[40,60])
*Main> main
(13882464,[28,35,18,19])
(11171160,[27,27,15,31])
-}
