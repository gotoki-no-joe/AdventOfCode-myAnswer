-- naive translation

i01 a b c d e f g h = i02 a 57 c d e f g h         -- set b 57
i02 a b c d e f g h = i03 a b b d e f g h          -- set c b
i03 0 b c d e f g h = i04 0 b c d e f g h
i03 a b c d e f g h = i05 a b c d e f g h          -- jnz a 2 --> 05
i04 a b c d e f g h = i09 a b c d e f g h          -- j 5 --> 09
i05 a b c d e f g h = i06 a (b*100) c d e f g h    -- mul b 100    <- 03
i06 a b c d e f g h = i07 a (b+100000) c d e f g h -- sub b -100000
i07 a b c d e f g h = i08 a b b d e f g h          -- set c b
i08 a b c d e f g h = i09 a b (c+17000) d e f g h  -- sub c -17000
i09 a b c d e f g h = i10 a b c d e 1 g h          -- set f 1 <- 32
i10 a b c d e f g h = i11 a b c 2 e f g h          -- set d 2
i11 a b c d e f g h = i12 a b c d 2 f g h          -- set e 2 <- 24
i12 a b c d e f g h = i13 a b c d e f d h          -- set g d <- 20
i13 a b c d e f g h = i14 a b c d e f (g*e) h      -- mul g e
i14 a b c d e f g h = i15 a b c d e f (g-b) h      -- sub g b
i15 a b c d e f 0 h = i16 a b c d e f 0 h
i15 a b c d e f g h = i17 a b c d e f g h          -- jnz g 2 --> 17
i16 a b c d e f g h = i17 a b c d e 0 g h          -- set f 0
i17 a b c d e f g h = i18 a b c d (e+1) f g h      -- sub e -1 <- 15
i18 a b c d e f g h = i19 a b c d e f e h          -- set g e
i19 a b c d e f g h = i20 a b c d e f (g-b) h      -- sub g b
i20 a b c d e f 0 h = i21 a b c d e f 0 h
i20 a b c d e f g h = i12 a b c d e f g h          -- jnz g -8 --> 12
i21 a b c d e f g h = i22 a b c (d+1) e f g h      -- sub d -1
i22 a b c d e f g h = i23 a b c d e f d h          -- set g d
i23 a b c d e f g h = i24 a b c d e f (g-b) h      -- sub g b
i24 a b c d e f 0 h = i25 a b c d e f 0 h
i24 a b c d e f g h = i11 a b c d e f g h          -- jnz g -13 -> 11
i25 a b c d e 0 g h = i26 a b c d e 0 g h
i25 a b c d e f g h = i27 a b c d e f g h          -- jnz f 2 -> 27
i26 a b c d e f g h = i27 a b c d e f g (h+1)      -- sub h -1
i27 a b c d e f g h = i28 a b c d e f b h          -- set g b <- 25
i28 a b c d e f g h = i29 a b c d e f (g-c) h      -- sub g c
i29 a b c d e f 0 h = i30 a b c d e f 0 h
i29 a b c d e f g h = i31 a b c d e f g h          -- jnz g 2 -> 31
i30 a b c d e f g h = h                            -- j 3 -> 33 == end
i31 a b c d e f g h = i32 a (b+17) c d e f g h     -- sub b -17
i32 a b c d e f g h = i09 a b c d e f g h          -- j -23 -> 9

run = i01 1 0 0 0 0 0 0 0
