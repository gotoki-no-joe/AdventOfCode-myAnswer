{-
もっと高速にしたいわ。
再帰呼び出しの底で(++)とか使っていると大変。
今まで出力したものを全て逆順に保存しておいて、
それを使いつくしたら、0はさんでそれを反転したものを出力する、という感じ。
-}

stream xs = result
  where
    result = xs ++ loop (length xs)
    loop len = '0' : map ollo (reverse $ take len result) ++ loop (len + len + 1)

ollo '0' = '1'
ollo '1' = '0'

test1 = take 20 $ stream "10000"

-- あとは同じで

checksum (x:y:xs) = (if x == y then '1' else '0') : checksum xs
checksum _ = []

check is size = fst $ until (odd . snd) enchecksum (is, size)

enchecksum (is, size) = (checksum is, div size 2)

compute s sz = check (take sz $ stream s) sz

test2 = compute "10000" 20

ans1 = compute "00111101111101000" 272

ans2 = compute "00111101111101000" 35651584

{- やっぱり大変。-}
