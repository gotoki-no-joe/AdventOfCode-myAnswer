{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Data.List
import Data.Either
import Control.Monad
import Debug.Trace
import qualified Data.Set as S

data Status = Status
  { total :: Int    -- total used mana
  , bhp :: Int      -- boss hp
  , hp :: Int       -- player
  , mana :: Int     -- player
  , shield :: Int   -- count
  , poison :: Int   -- count
  , recharge :: Int -- count
 } deriving (Eq, Ord, Show) -- , Generic, NFData)

bossDamage = 9

splMissile stat
  | mana stat < 53 = Nothing
  | True = Just ( stat { mana = mana stat - 53, bhp = bhp stat - 4
                       , total = total stat + 53 } )

splDrain stat
  | mana stat < 73 = Nothing
  | True = Just ( stat { mana = mana stat - 73, hp = hp stat + 2, bhp = bhp stat - 2
                       , total = total stat + 73 } )

splShield stat
  | mana stat < 113 || shield stat > 0 = Nothing
  | True = Just ( stat { mana = mana stat - 113, shield = 6, total = total stat + 113 } )

splPoison stat
  | mana stat < 173 || poison stat > 0 = Nothing
  | True = Just ( stat { mana = mana stat - 173, poison = 6, total = total stat + 173 } )

splRecharge stat
  | mana stat < 229 || recharge stat > 0 = Nothing
  | True = Just ( stat { mana = mana stat - 229, recharge = 5, total = total stat + 229 } )

countdown stat = stat
  { shield = max 0 (shield stat - 1)
  , poison = max 0 (poison stat - 1)
  , recharge = max 0 (recharge stat - 1)
  , bhp = bhp stat - if (poison stat > 0) then 3 else 0
  , mana = mana stat + if (recharge stat > 0) then 101 else 0
  }

bossAttack stat
  | hp1 > 0 = Just ( stat { hp = hp1 } )
  | otherwise = Nothing
  where
    hp1 = hp stat - max 1 (bossDamage - if shield stat > 0 then 7 else 0)

initial = Status
  { hp = 50 , mana = 500
  , shield = 0 , poison = 0 , recharge = 0
  , bhp = 51
  , total = 0 }

m2l (Just x) = [x]
m2l Nothing = []

{-
Player turn
1. countdown : poisonで決着する場合あり
2. spell : missile, drainで決着する場合あり
Boss turn
3. countdown : poisonで決着する場合あり
4. attack : 死亡での枝刈りあり

この中で2だけ可変で、あとは関数であること、
初回の1は何も起きないこと、から
3,4,1をした結果を求める関数を作り、
2の結果にmapする

bhp <= 0 で勝ちのとき、totalの候補が出る
hp <= 0 で負けのとき、それは捨てる
どちらでもない状態は、以降で展開するためにinsertする

-}

compute1 :: Int -- 最小値
         -> S.Set Status -- 調べる状態リスト
         -> Int -- 結果
compute1 curTotal stats0
  | null stats0 = curTotal
  | curTotal < total stat = curTotal
  | True = (compute1 $! ct2) $! conts2 where
    (stat,stats) = S.deleteFindMin stats0
    (ends,conts) = partitionEithers (
      do
        f <- [splMissile, splDrain, splShield, splPoison, splRecharge]
        stat2 <- m2l $ f stat
        if bhp stat2 <= 0 then return (Left $ total stat2) else do
          let stat3 = countdown stat2
          if bhp stat3 <= 0 then return (Left $ total stat3) else do
            stat4 <- m2l $ bossAttack stat3
            let stat5 = countdown stat4
            if bhp stat5 <= 0 then return (Left $ total stat5) else return (Right stat5)
     )
    ct2 = minimum (curTotal : ends)
    conts2 = foldr S.insert stats conts

ans1 = compute1 maxBound (S.singleton initial)

test x = compute1 maxBound (S.singleton (initial { bhp = x }))

{-
O(n)なinsertによるordered listからData.Setに差し替えたら、
答えあっという間に出たんだがww

*Main> test 10
159
*Main> test 20
265
*Main> test 30
332
*Main> test 40
734
*Main> ans1
900
-}

hardinitial = initial { hp = hp initial - 1 }

compute2 :: Int -- 最小値
         -> S.Set Status -- 調べる状態リスト
         -> Int -- 結果
compute2 curTotal stats0
  | null stats0 = curTotal
  | curTotal < total stat = curTotal
  | True = (compute2 $! ct2) $! conts2 where
    (stat,stats) = S.deleteFindMin stats0
    (ends,conts) = partitionEithers (
      do
        f <- [splMissile, splDrain, splShield, splPoison, splRecharge]
        stat2 <- m2l $ f stat
        if bhp stat2 <= 0 then return (Left $ total stat2) else do
          let stat3 = countdown stat2
          if bhp stat3 <= 0 then return (Left $ total stat3) else do
            stat4 <- m2l $ bossAttack stat3
            let stat5 = stat4 { hp = hp stat4 - 1 }
            guard $ hp stat5 > 0
            let stat6 = countdown stat5
            if bhp stat6 <= 0 then return (Left $ total stat6) else return (Right stat6)
     )
    ct2 = minimum (curTotal : ends)
    conts2 = foldr S.insert stats conts

ans2 = compute2 maxBound (S.singleton hardinitial)

test2 x = compute2 maxBound (S.singleton $ hardinitial { bhp = x })

{-
Data.Setならこっちも一瞬だった…

*Main> test2 10
159
*Main> test2 20
265
*Main> test2 30
332
*Main> test2 40
754
*Main> ans2
1216
-}
