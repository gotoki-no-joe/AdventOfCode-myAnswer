import Data.List

pHP = 100

enHP = 103
enDam = 9
enArm = 2

--    Cost  Damage  Armor
weapons =
  [( 8, 4, 0) -- Dagger
  ,(10, 5, 0) -- Shortsword
  ,(25, 6, 0) -- Warhammer
  ,(40, 7, 0) -- Longsword
  ,(74, 8, 0) -- Greataxe
  ]

armors =
  [( 0, 0, 0) -- none
  ,(13, 0, 1) -- Leather
  ,(31, 0, 2) -- Chainmail
  ,(53, 0, 3) -- Splintmail
  ,(75, 0, 4) -- Bandedmail
  ,(102,0, 5) -- Platemail
  ]

rings =
  [(25, 1, 0) -- Damage +1
  ,(50, 2, 0) -- Damage +2
  ,(100,3, 0) -- Damage +3
  ,(20, 0, 1) -- Defense +1
  ,(40, 0, 2) -- Defense +2
  ,(80, 0, 3) -- Defense +3
  ]

ringChoice = (0,0,0) : rings ++
   [ (c1+c2,d1+d2,a1+a2)
   | (c1,d1,a1):r1 <- tails rings
   , (c2,d2,a2) <- r1
   ]

allResult = sort
  [ (wc+ac+rc, result)
  | (wc,wd,wa) <- weapons
  , (ac,ad,aa) <- armors
  , (rc,rd,ra) <- ringChoice
  , let result = battle (wd+ad+rd) (wa+aa+ra)
  ]

battle pd pa = turnA pHP enHP where
  px = max (pd - enArm) 1
  ex = max (enDam - pa) 1
  turnA php enhp
    | php <= 0 = (False, php) -- (php, enhp) -- lose
    | otherwise = turnB php (enhp - px)
  turnB php enhp
    | enhp <= 0 = (True, php) -- (php, enhp) -- win
    | otherwise = turnA (php - ex) enhp

ans1 = head $ filter (fst.snd) allResult

ans2 = last $ filter (not.fst.snd) allResult

{-
*Main> ans1
(121,(True,2))
*Main> ans2
(201,(False,0))

武器はマストだというのを忘れていた。ちっ。
-}
