-- ハードコードする。

f00 a b c d = f01 1 b c d -- cpy 1 a
f01 a b c d = f02 a 1 c d -- cpy 1 b
f02 a b c d = f03 a b c 26 -- cpy 26 d
f03 a b c d = (if c /= 0 then f05 else f04) a b c d -- jnz c 2
f04 = f09 -- jnz 1 5
f05 a b c d = f06 a b 7 d -- cpy 7 c
f06 a b c d = f07 a b c (succ d) -- inc d
f07 a b c d = f08 a b (pred c) d -- dec c
f08 a b c d = (if c /= 0 then f06 else f09) a b c d -- jnz c -2
f09 a b c d = f10 a b a d -- cpy a c
f10 a b c d = f11 (succ a) b c d -- inc a
f11 a b c d = f12 a (pred b) c d -- dec b
f12 a b c d = (if b /= 0 then f10 else f13) a b c d -- jnz b -2
f13 a b c d = f14 a c c d -- cpy c b
f14 a b c d = f15 a b c (pred d) -- dec d
f15 a b c d = (if d /= 0 then f09 else f16) a b c d -- jnz d -6
f16 a b c d = f17 a b 13 d -- cpy 13 c
f17 a b c d = f18 a b c 14 -- cpy 14 d
f18 a b c d = f19 (succ a) b c d -- inc a
f19 a b c d = f20 a b c (pred d) -- dec d
f20 a b c d = (if d /= 0 then f18 else f21) a b c d -- jnz d -2
f21 a b c d = f22 a b (pred c) d -- dec c
f22 a b c d = (if c /= 0 then f17 else end) a b c d -- jnz c -5
end a b c d = a

part1 = f00 0 0 0 0

part2 = f00 0 0 1 0

-- 圧倒的に軽く速くはなったが、
-- 一つ引数を置き間違えただけで酷いバグになった。
