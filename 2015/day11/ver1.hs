{-
大人しく作るしかないよね。
ただし逆順にする方がいい。
-}

increment "" = ""
increment ('z':cs) = 'a':increment cs
increment (c:cs) = mysucc c : cs where
  mysucc 'h' = 'j' -- cancel 'i'
  mysucc 'n' = 'p' -- cancel 'o'
  mysucc 'k' = 'm' -- cancel 'l'
  mysucc c   = succ c

-- foldr使えば末尾からできる？

increment2 cs = snd $  foldr f (True,"") cs where
  f c (False, ds) = (False, c:ds)
  f c (True , ds)
    | elem c "ilo" = (False, succ (succ c):ds)
    | c == 'z'     = (True ,           'a':ds)
    | otherwise    = (False,       succ c :ds)

-- 規則1は、二つ含んでいたらいかんとは言ってないよね。
-- exactly one ではないから。
-- at least とも書いてくれなかったけど。

requirement1 cs = or $ zipWith (&&) eq12 eq13 where
  css = map succ cs
  csp = map pred cs
  eq12 = zipWith (==) css (tail cs)
  eq13 = zipWith (==) css (drop 2 csp)

-- 規則2はチェックする代わりに、初期に入っていたらそれを全部捨てるようにしよう。

kill_ilo (c:cs)
  | elem c "ilo" = succ c : map (const 'a') cs
  | otherwise    = c : kill_ilo cs

requirement2 cs = notElem 'i' cs && notElem 'l' cs && notElem 'o' cs

-- 規則3はaaを見つけて、aaaを無視する。どうやって？でもaaaaはいいんだよね。
requirement3 cs = length eqs >= 2 && last eqs - head eqs > 1 where
  eqs = [ i | (i,eq) <- zip [0..] $ zipWith (==) cs (tail cs), eq ]

test cs = (requirement1 cs, requirement2 cs, requirement3 cs)
test1 = "hijklmmn"
test2 = "abbceffg"
test3 = "abbcegjk"
test4 = "abcdefgh"
test5 = "ghijklmn"
input = "hxbxwxba"

nextpass cs = loop cs1 where
  cs1 = if not (requirement2 cs) then kill_ilo cs else (increment2 cs)
  loop cs
    | requirement1 cs && requirement3 cs = cs
    | otherwise = loop (increment2 cs)

ans1 = nextpass input
ans2 = nextpass ans1
