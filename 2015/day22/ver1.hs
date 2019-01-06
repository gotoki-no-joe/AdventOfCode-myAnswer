import Control.Monad
import Debug.Trace
import Data.Set

{-
何とも面倒な探索問題やの。
-}

-- Hit Points: 51
-- Damage: 9

-- HP, Mana, Shield, Poison, Recharge
data Status = Status
  { hp :: Int       -- player
  , mana :: Int     -- player
  , shield :: Int   -- count
  , poison :: Int   -- count
  , recharge :: Int -- count
  , bhp :: Int      -- boss
  , bdmg :: Int     -- boss damage score
  , total :: Int    -- total used mana
 } deriving Show

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
    hp1 = hp stat - max 1 (bdmg stat - if shield stat > 0 then 7 else 0)

initial = Status
  { hp = 50 , mana = 500
  , shield = 0 , poison = 0 , recharge = 0
  , bhp = 51, bdmg = 9
  , total = 0 }

m2l (Just x) = [x]
m2l Nothing = []

{-

ans1 = minimum $ iter initial

iter :: Status -> [Int]
iter stat = do -- trace (show stat) $ do
  let stat1 = countdown stat
  if bhp stat1 <= 0 then return (total stat1) else do
    f <- [splMissile, splDrain, splShield, splPoison, splRecharge]
    stat2 <- m2l $ f stat1
    if bhp stat2 <= 0 then trace (show $ total stat2) $ return (total stat2) else do
      let stat3 = countdown stat2
      if bhp stat3 <= 0 then trace (show $ total stat3) $ return (total stat3) else do
        stat4 <- m2l $ bossAttack stat3
        guard $ hp stat4 > 0
        iter stat4

-}

{-

これでは、合流が激しくて同じ計算の重複であふれてしまう。
深さ優先探索で、計算済みの状態を弾く最適化が必要だ。
それともそれは、集合ではなくその状態に至る最小のtotalを記録するMapか？
でもそうだと、それが覆されたときに面倒だ。

hp   mana   countdown   bhp  total
51 x 500+ x 7 x 7 x 7 x 52 x ?
大した数ではない気がするのだが。

いや、単に recharge し続けるアホのせいで終わらないのか？
bhpやtotalの小さい順に、優先順位つき探索を行って、
一つでも結果が出たら、それよりも中間結果のtotalが上回るものは却下して
有限にできる。そのフィルターありとなしで2つのモードができるのか？

-}
