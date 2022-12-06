# 22日目：魔法使いシミュレータ 20XX

Little Henry Caseは、刀や杖でボスを倒すのは退屈だと判断しました。
今、彼は魔法使いでゲームをしています。
もちろん、彼は別のボスに立ち往生しているし、もう一度あなたの助けが必要です。

このバージョンでは、プレイヤーとボスが交互に交代しながら戦闘が進行します。
プレイヤーはやはり先に行動します。
しかし今、あなたは装備を持っていません。
代わりに、あなたは唱えるためにあなたの呪文の1つを選ばなければなりません。
先にヒットポイント0以下になったキャラクターが負けます。

あなたは魔法使いなので、あなたは鎧を着ることができず、
あなたは普通に攻撃することはできません。
しかし、あなたは**魔法のダメージを与える**ので、
あなたの対戦相手の装甲は無視され、そのためボスも事実上装甲は零です。
以前のように、（この場合、呪文からの）装甲がダメージを1未満に減少させるのであれば、
それは代わりに1になります。
つまり、ボスの攻撃は常に少なくとも1ダメージを与えます。

あなたの各ターンに、
あなたは唱えるためにあなたの呪文の一つを選ぶ必要があります。
呪文を唱える余裕がない場合、あなたは負けます。
呪文は**マナ**を消費します。
あなたは**500**マナから始めますが、上限はありません。
あなたは呪文を唱えるのに十分なマナを持っていなければならず、
あなたがそれを唱えたときにそのコストは即座に差し引かれます。
あなたの呪文はマジックミサイル、ドレイン、シールド、ポイズン、リチャージです。

- **マジックミサイル**は53マナかかります。即座に4ダメージを与えます。
- **ドレイン**は73マナがかかります。
それは即座に2ダメージを与え、2ヒットポイントを回復します。
- **シールド**は113マナがかかります。
それは6ターン持続する**効果**を開始します。
それがアクティブになっている間、あなたのアーマースコアは7増加します。
- **ポイズン**は173マナかかります。
それは6ターン持続する**効果**を開始します。
それがアクティブである間、各ターンの開始時に、それはボスに3ダメージを与えます。
- **リチャージ**は229マナかかります。
それは5ターン持続する**効果**を開始します。
それがアクティブになっている間、各ターンの開始時に、
それはあなたに新しいマナを101与えます。

**効果**はすべて同じように機能します。
効果はプレイヤーのターンとボスのターンの両方の開始時に適用されます。
効果はタイマー（それらが持続するターン数）を伴って作成されます。
各ターンの開始時に、彼らが持っている効果を適用した後、
それらのタイマーは1減少する。
これによりタイマーが零になると、効果は終了します。
あなたはすでにアクティブになっている効果を開始する呪文を唱えることはできません。
ただし、効果はそれらが終了するのと同じターンに開始することができます。

たとえば、プレイヤーに10ヒットポイントと250マナがあり、
ボスに13ヒットポイントと8ダメージがあるとします。

~~~
-- Player turn --
- Player has 10 hit points, 0 armor, 250 mana
- Boss has 13 hit points
Player casts Poison.

-- Boss turn --
- Player has 10 hit points, 0 armor, 77 mana
- Boss has 13 hit points
Poison deals 3 damage; its timer is now 5.
Boss attacks for 8 damage.

-- Player turn --
- Player has 2 hit points, 0 armor, 77 mana
- Boss has 10 hit points
Poison deals 3 damage; its timer is now 4.
Player casts Magic Missile, dealing 4 damage.

-- Boss turn --
- Player has 2 hit points, 0 armor, 24 mana
- Boss has 3 hit points
Poison deals 3 damage. This kills the boss, and the player wins.
~~~

それでは、
ボスが代わりに14ヒットポイントを持っていることを除いて、
同じ初期条件を仮定します。

~~~
-- Player turn --
- Player has 10 hit points, 0 armor, 250 mana
- Boss has 14 hit points
Player casts Recharge.

-- Boss turn --
- Player has 10 hit points, 0 armor, 21 mana
- Boss has 14 hit points
Recharge provides 101 mana; its timer is now 4.
Boss attacks for 8 damage!

-- Player turn --
- Player has 2 hit points, 0 armor, 122 mana
- Boss has 14 hit points
Recharge provides 101 mana; its timer is now 3.
Player casts Shield, increasing armor by 7.

-- Boss turn --
- Player has 2 hit points, 7 armor, 110 mana
- Boss has 14 hit points
Shield's timer is now 5.
Recharge provides 101 mana; its timer is now 2.
Boss attacks for 8 - 7 = 1 damage!

-- Player turn --
- Player has 1 hit point, 7 armor, 211 mana
- Boss has 14 hit points
Shield's timer is now 4.
Recharge provides 101 mana; its timer is now 1.
Player casts Drain, dealing 2 damage, and healing 2 hit points.

-- Boss turn --
- Player has 3 hit points, 7 armor, 239 mana
- Boss has 12 hit points
Shield's timer is now 3.
Recharge provides 101 mana; its timer is now 0.
Recharge wears off.
Boss attacks for 8 - 7 = 1 damage!

-- Player turn --
- Player has 2 hit points, 7 armor, 340 mana
- Boss has 12 hit points
Shield's timer is now 2.
Player casts Poison.

-- Boss turn --
- Player has 2 hit points, 7 armor, 167 mana
- Boss has 12 hit points
Shield's timer is now 1.
Poison deals 3 damage; its timer is now 5.
Boss attacks for 8 - 7 = 1 damage!

-- Player turn --
- Player has 1 hit point, 7 armor, 167 mana
- Boss has 9 hit points
Shield's timer is now 0.
Shield wears off, decreasing armor by 7.
Poison deals 3 damage; its timer is now 4.
Player casts Magic Missile, dealing 4 damage.

-- Boss turn --
- Player has 1 hit point, 0 armor, 114 mana
- Boss has 2 hit points
Poison deals 3 damage. This kills the boss, and the player wins.
~~~

あなたは**50ヒットポイント**と**500マナポイント**から始めます。
ボスの実際の属性値はあなたのパズルの入力にあります。
あなたが使うことができ、それでも戦いに勝てる**マナの最小量**は何ですか？
（リチャージの効果を負のマナを「支出する」として含めないでください。）

# パート2

次回のゲームでは、難易度を「ハード」に上げます。

各**プレイヤーターン**の開始時（他の効果が適用される前）に、
あなたは1ヒットポイントを失います。
これで0ヒットポイント以下になった場合、あなたは負けます。

あなたとボスの開始属性値は上と同じで、
あなたが使うことができて、まだ戦いに勝つことができる**マナの最小量**は何ですか？
