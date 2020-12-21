# 15日目：無茶な暗唱 #

あなたは空港シャトルに乗り、あなたの休暇の島への新しいフライトを予約しようとします。
嵐のため、すべての直行便はキャンセルされましたが、嵐を回避する経路があります。あなたはそれを取ります。

フライトを待つ間、北極に置いてきた妖精たちに連絡を入れることにしました。
彼らは**記憶ゲーム**で遊んでいて、興奮のあまりルールを説明しだしました！

このゲームでは、プレイヤーは順番に数字を言います。
**はじめの数リスト**（あなたのパズル入力）を順番に読み上げることで開始します。
以降の各ターンは、**最後に呼ばれた数**について考えることになります。

- その数が**初めて**呼ばれたところならば、次の人は0と発声します。
- そうでなく、その数が以前にも呼ばれているならば、次の人は、前回呼ばれたときと**何ターン離れているか**を答えます。

したがって、はじめの数リストが終わってから、
各ターンではプレイヤーは**0**（最後の数が新規の場合）
または**年齢**（最後の数が再登場の場合）のいずれかを声に出します。

たとえば、初めの数リストが`0,3,6`であるとします。

- ターン1：1番目に呼ばれる数は、はじめの数リストの0です。
- ターン2：2番目に呼ばれる数は、はじめの数リストの3です。
- ターン3：3番目に呼ばれる数は、はじめの数リストの6です。
- ターン4：ここで、最後の数6を考えます。この数が呼ばれたのはこれが初めてなので、4番目に呼ばれる数は0です。
- ターン5：次に、また最後の数を考えます。それは0で、これは前にも呼ばれているので、
最後に呼ばれたターン(前のターン4)と、その前に呼ばれたときのターン(ターン1)の差を次は呼ぶことになります。
したがって、5番目に呼ばれる数は4 - 1 = 3です。
- ターン6：最後に呼ばれた数3もまた以前にも呼ばれたことがあり、最近ではターン5と2です。
したがって、6番目の数は 5 - 2 = 3です。
- ターン7：3が今2回続けて呼ばれたところなので、その2つのターンは1離れているため、7番目の数は1です。
- ターン8：1は初めてなので、8番目の数は0です。
- ターン9：0はターン8と4で最後に呼ばれたので、9番目の数はそれらの間の差4です。
- ターン10：4は初めてなので、10番目の数は0です。

（ゲームは、妖精たちが遊びに飽きるか、夕食の準備ができるか、どちらか早い方で終了します。）

彼らのあなたへの質問は：**2020**番目に呼ばれる数は何でしょうか？
上記の例では、2020番目の数は436になります。

さらにいくつかの例を示します。

- はじめの数リストが1,3,2のとき、2020番目の数は1です。
- はじめの数リストが2,1,3のとき、2020番目の数は10です。
- はじめの数リストが1,2,3のとき、2020番目の数は27です。
- はじめの数リストが2,3,1のとき、2020番目の数は78です。
- はじめの数リストが3,2,1のとき、2020番目の数は438です。
- はじめの数リストが3,1,2のとき、2020番目の数は1836です。

あなたのはじめの数リストについて、**2020番目に呼ばれる数は何ですか？**

あなたのパズル入力は0,13,1,16,6,17です。

# パート2 #

感銘を受けた妖精はあなたに挑戦します：30000000番目に呼ばれる数は何でしょう。
たとえば、上記と同じはじめの数リストがあるとき、

- 0,3,6が与えられたとき、30000000番目の数は175594です。
- 1,3,2が与えられたとき、30000000番目の数は2578です。
- 2,1,3が与えられたとき、30000000番目の数は3544142です。
- 1,2,3が与えられたとき、30000000番目の数は261214です。
- 2,3,1が与えられたとき、30000000番目の数は6895259です。
- 3,2,1が与えられたとき、30000000番目の数は18です。
- 3,1,2が与えられたとき、30000000番目の数は362です。

あなたのはじめの数リストについて、**30000000番目に呼ばれる数は何ですか？**