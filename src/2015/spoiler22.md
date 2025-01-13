コードにコメントを散々つけたので、それで読み解いて。
基本的に、忠実にシミュレーションを実行していく、
最小コストのものを見つけたいので、状態を保存する入れ物として

- スタック（LIFO, DFSになる）
- キュー（FIFO, BFSになる）
- 優先度付きキュー（A*探索と呼ぶらしい）

の最後のそれを使って、さらに、同じ状態を重複して計算しないように `Set` を使った。

```haskell
import qualified Data.Set as S
import Data.Either
import Control.Monad

-- ゲームの各種状態を保持するレコード
data Status = Status
  { total :: Int    -- total used mana
  , bhp :: Int      -- boss hp
  , hp :: Int       -- player
  , mana :: Int     -- player
  , shield :: Int   -- count
  , poison :: Int   -- count
  , recharge :: Int -- count
  } deriving (Eq, Ord)
-- total を小さく抑えて、自分が勝てる方法があるか調べたいので、
-- totalの昇順キューを使ったA*探索で勝ち筋を探す。
-- 状況が重複する可能性があるので、Data.HeapでなくData.Setを用いる。
-- そのためtotalが先頭で、Ord型クラスが必要。

-- input.txt で与えられたパラメータ
bossHP = 51 :: Int
bossDamage = 9 :: Int

-- 自分の行動で起きる状態変化、または行動できないときNothing

-- マジックミサイル
splMissile :: Status -> Maybe Status
splMissile stat
  | mana stat < 53 = Nothing
  | True = Just ( stat { mana = mana stat - 53, bhp = bhp stat - 4
                       , total = total stat + 53 } )

-- ドレイン
splDrain :: Status -> Maybe Status
splDrain stat
  | mana stat < 73 = Nothing
  | True = Just ( stat { mana = mana stat - 73, hp = hp stat + 2, bhp = bhp stat - 2
                       , total = total stat + 73 } )

-- シールド
splShield :: Status -> Maybe Status
splShield stat
  | mana stat < 113 || shield stat > 0 = Nothing
  | True = Just ( stat { mana = mana stat - 113, shield = 6, total = total stat + 113 } )

-- ポイズン
splPoison :: Status -> Maybe Status
splPoison stat
  | mana stat < 173 || poison stat > 0 = Nothing
  | True = Just ( stat { mana = mana stat - 173, poison = 6, total = total stat + 173 } )

-- リチャージ
splRecharge :: Status -> Maybe Status
splRecharge stat
  | mana stat < 229 || recharge stat > 0 = Nothing
  | True = Just ( stat { mana = mana stat - 229, recharge = 5, total = total stat + 229 } )

-- ターン開始時のカウントダウン
countdown :: Status -> Status
countdown stat = stat
  { shield = max 0 (shield stat - 1)
  , poison = max 0 (poison stat - 1)
  , recharge = max 0 (recharge stat - 1)
  , bhp = bhp stat - if poison stat > 0 then 3 else 0
  , mana = mana stat + if recharge stat > 0 then 101 else 0
  }

-- 敵ボスからの攻撃
-- 自分が負ける場合はNothing
bossAttack :: Status -> Maybe Status
bossAttack stat
  | hp1 > 0 = Just ( stat { hp = hp1 } )
  | otherwise = Nothing
  where
    hp1 = hp stat - max 1 (bossDamage - if shield stat > 0 then 7 else 0)

-- 初期状態
initial :: Status
initial = Status
  { hp = 50 , mana = 500
  , shield = 0 , poison = 0 , recharge = 0
  , bhp = bossHP
  , total = 0 }

{-
戦闘の過程で起きること

Player turn
1. countdown : poisonで決着する場合あり
2. spell : missile, drainで決着する場合あり
Boss turn
1. countdown : poisonで決着する場合あり
2. attack : 死亡での枝刈りあり

1,3,4は起きることは固定的、2は行動の選択肢がある
初期状態で1は何も起きない
ので、状態キューには1が終わった段階のものを格納し、
totalの小さい順に、2の選択肢ごとに状態の枝分かれを考える。
- 自分が死ぬ、MPが足りずにできない行動については捨てる
- ボスを倒す結果なら、最小結果を更新する(*)
- まだ続くなら、キューに追加する

(*)で最初に見つかったものが答えとは限らない。
キューでは後塵を拝しているけれど、次のターンで記録更新して勝利する場合もありうるから、
「これまでのベストスコア」を保持してループする。

キューの内容が（なにもまだしていないのに）これを上回ったら、その時点で探索は完了。
-}

compute1 :: Int -- 最小値
         -> S.Set Status -- 調べる状態リスト
         -> Int -- 結果
compute1 curTotal stats0
  | null stats0 = curTotal             -- 普通はないけれど、キューが空になったときはそれで終わり
  | curTotal < total stat = curTotal   -- これ以上調べなくていい状態になったら終わり
  | otherwise = compute1 ct2 conts2    -- 状態遷移して続ける
      where
-- キューの先頭を取り出す
        (stat,stats) = S.deleteFindMin stats0
-- 行動によって異なる結果を仕分ける
        (ends,conts) = partitionEithers $
          do
-- プレイヤー行動
            Just stat1 <- map ($ stat) [splMissile, splDrain, splShield, splPoison, splRecharge]
-- ボスを倒せたらLeftに
            if bhp stat1 <= 0 then return (Left $ total stat1) else do
-- さもなくば続ける、ボスターンのカウントダウン
              let stat2 = countdown stat1
-- カウントダウンでボスが倒れたらLeftに
              if bhp stat2 <= 0 then return (Left $ total stat2) else do
-- さもなくば続けて、ボスの攻撃
                Just stat3 <- [bossAttack stat2]
-- 次のターンに入って、プレイヤーターンのカウントダウン
                let stat4 = countdown stat3
-- カウントダウンでボスが倒れたらLeftに まだ続くならRightに
                if bhp stat4 <= 0 then return (Left $ total stat4) else return (Right stat4)
-- ベストスコアを更新
        ct2 = minimum (curTotal : ends)
-- 続ける場合をキューに追加
        conts2 = S.union stats $ S.fromList conts

-- パート1答え
part1 = compute1 maxBound (S.singleton initial)

-- パート2

-- パート1では初期状態でプレイヤーカウントダウンでは何も起きないのでそのまま入れたが、
-- 今回はプレイヤーカウントダウンでHPが1減って開始

initial2 = initial { hp = hp initial - 1 }

-- 基本的にcompute1と同じ。プレイヤーカウントダウン前にHP-1処理を挿入

compute2 :: Int -- 最小値
         -> S.Set Status -- 調べる状態リスト
         -> Int -- 結果
compute2 curTotal stats0
  | null stats0 = curTotal             -- 普通はないけれど、キューが空になったときはそれで終わり
  | curTotal < total stat = curTotal   -- これ以上調べなくていい状態になったら終わり
  | otherwise = compute2 ct2 conts2    -- 状態遷移して続ける
      where
-- キューの先頭を取り出す
        (stat,stats) = S.deleteFindMin stats0
-- 行動によって異なる結果を仕分ける
        (ends,conts) = partitionEithers $
          do
-- プレイヤー行動
            Just stat1 <- map ($ stat) [splMissile, splDrain, splShield, splPoison, splRecharge]
-- ボスを倒せたらLeftに
            if bhp stat1 <= 0 then return (Left $ total stat1) else do
-- さもなくば続ける、ボスターンのカウントダウン
              let stat2 = countdown stat1
-- カウントダウンでボスが倒れたらLeftに
              if bhp stat2 <= 0 then return (Left $ total stat2) else do
-- さもなくば続けて、ボスの攻撃
                Just stat3 <- [bossAttack stat2]
-- パート2固有
-- 次のターンに入って、まずプレイヤーはHPを1失う。ここで倒れるならこの選択は捨てる
                let stat3a = stat3 { hp = hp stat3 - 1 }
                guard $ hp stat3a > 0
-- パート2固有終わり
-- 次のターンに入って、プレイヤーターンのカウントダウン
                let stat4 = countdown stat3a
-- カウントダウンでボスが倒れたらLeftに まだ続くならRightに
                if bhp stat4 <= 0 then return (Left $ total stat4) else return (Right stat4)
-- ベストスコアを更新
        ct2 = minimum (curTotal : ends)
-- 続ける場合をキューに追加
        conts2 = S.union stats $ S.fromList conts

-- パート2答え
part2 = compute2 maxBound (S.singleton initial2)
```
