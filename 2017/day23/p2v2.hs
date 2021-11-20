i09 b c d e f g h = i11 b c 2 e 1 g h          -- set f 1 <- 32
i11 b c d e f g h = i12 b c d 2 f g h          -- set e 2 <- 24
i12 b c d e f g h = i15 b c d e f (d*e-b) h    -- <- 20
i15 b c d e f 0 h = i20 b c d (e+1) 0 (e+1-b) h
i15 b c d e f g h = i20 b c d (e+1) f (e+1-b) h
i20 b c d e f 0 h = i24 b c (d+1) e f (d+1-b) h
i20 b c d e f g h = i12 b c d e f g h          -- jnz g -8 --> 12
i24 b c d e f 0 h = i25 b c d e f 0 h
i24 b c d e f g h = i11 b c d e f g h          -- jnz g -13 -> 11
i25 b c d e 0 g h = i29 b c d e 0 (b-c) (h+1)
i25 b c d e f g h = i29 b c d e f (b-c) h          -- if f == 0 then h = h + 1
i29 b c d e f 0 h = h
i29 b c d e f g h = i09 (b+17) c d e f g h

run = i09 105700 122700 0 0 0 0 0

{-
このような局所最適化だけでは乗り切れなくて、何を計算しているかを見抜く必要がある。
-}
