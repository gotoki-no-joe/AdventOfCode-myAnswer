# 13日目：シャトル検索 #

あなたのフェリーは近くの港に安全に行くことができますが、それ以上進むことはありません。
別の船を予約するために電話をかけようとしたとき、
その港から休暇の島に向かう船がないことに気づきました。
港から最寄りの空港まで行く必要があります。

幸いなことに、港から空港までのシャトルバスサービスを利用できます。
各バスにはID番号があり、この番号はバスが**空港に向けて出発する頻度**も示しています。

バスのスケジュールは、過去の固定基準点からの経過時間（**分単位**）を測定する**タイムスタンプ**に基づいて定義されます。
タイムスタンプ0に、すべてのバスが同時に港から出発しました。
その後、各バスは空港、次いで他のさまざまな場所に移動し、最後に港に戻ってその旅を永遠に繰り返します。

それぞれのバスが回るこのループにかかる時間は、そのバスのID番号でもあります。
ID`5`を持つバスはタイムスタンプ0,5,10,15,…に港を出発します。
ID`11`を持つバスは0,11,22,33,…に出発します。
バスが出発するときにそこにいるなら、あなたはそのバスに乗って空港に行くことができます！

あなたのメモ（パズル入力）は2行で構成されています。
最初の行は、**あなたがバスで出発できる最も早いタイムスタンプ**の見積もりです。
2行目は、シャトル会社から提供された、運行されているバスIDの一覧です。
`x`と示されている項目は運休しているので、無視することにします。

到着後の時間を節約するために、あなたの目標は
**あなたが空港に行くことができる最も早いバス**を見つけることです。
（そのようなバスは1つだけです。）

たとえば、次のメモがあるとします。

~~~
939
7,13,x,x,59,x,31,19
~~~

あなたがバスで出発できる最も早いタイムスタンプは`939`で、
運航しているバスのIDは`7`, `13`, `59`, `31`, `19`です。
タイムスタンプ939の前後で、これらのIDのバスは`D`と印した時刻に出発します。

~~~
time   bus 7   bus 13  bus 59  bus 31  bus 19
929      .       .       .       .       .
930      .       .       .       D       .
931      D       .       .       .       D
932      .       .       .       .       .
933      .       .       .       .       .
934      .       .       .       .       .
935      .       .       .       .       .
936      .       D       .       .       .
937      .       .       .       .       .
938      D       .       .       .       .
939      .       .       .       .       . (強調)
940      .       .       .       .       .
941      .       .       .       .       .
942      .       .       .       .       .
943      .       .       .       .       .
944      .       .       D       .       . (強調)
945      D       .       .       .       .
946      .       .       .       .       .
947      .       .       .       .       .
948      .       .       .       .       .
949      .       D       .       .       .
~~~

あなたが乗ることができる最も早いバスはバスID`59`です。
タイムスタンプ`944`まで出発しないため、
出発するまで944 - 939 = 5分待つ必要があります。
バスIDに待ち時間の分を掛けると295が得られます。

**空港まで行くことができる最も早いバスのIDに、
そのバスを待つのに必要な分数を掛けたものは何ですか？**

# パート2 #

シャトル会社はコンテストを開催しています。
最初のバスIDがその時刻に出発し、以降リストの後続の各バスIDが毎分出発するような、
最も早いタイムスタンプを見つけることができた人は全員金貨1枚が貰えます。
（入力の最初の行はここでは意味を持ちません。）

たとえば、上記と同じバスIDのリストがあるとします。

~~~
7,13,x,x,59,x,31,19
~~~

スケジュール中の`x`は、その時刻に出発するバスのIDについて制約がないことを意味します。

これは、次のような最も早いタイムスタンプ（`t`とする）を探していることを意味します。

- バスID`7`はタイムスタンプ`t`に出発します。
- バスID`13`はタイムスタンプ`t`の1分後に出発します。
- タイムスタンプ`t`の2分後と3分後の出発に関する要求や制限はありません。
- バスID`59`はタイムスタンプ`t`の4分後に出発します。
- タイムスタンプ`t`から5分後の出発に関する要求や制限はありません。
- バスID`31`はタイムスタンプ`t`の6分後に出発します。
- バスID`19`はタイムスタンプ`t`の7分後に出発します。

重要なバスの出発は、リストに挙げれらたIDのバスが`t`からの特定のオフセットで出発することだけです。
これらのバスIDは他の時間にも出発することが許され、他のバスIDがその時間に出発することも許されます。
例えば上記のリストで、
バスID`19`はバスID`7`の出発したタイムスタンプの7分後に出発する必要があるため、
バスID`7`は常にタイムスタンプ`t`の7分後にバスID`19`とともに**再度**出発します。

この例では、これが発生する最も早いタイムスタンプは`1068781`です。

~~~
time     bus 7   bus 13  bus 59  bus 31  bus 19
1068773    .       .       .       .       .
1068774    D       .       .       .       .
1068775    .       .       .       .       .
1068776    .       .       .       .       .
1068777    .       .       .       .       .
1068778    .       .       .       .       .
1068779    .       .       .       .       .
1068780    .       .       .       .       .
1068781    D       .       .       .       .
1068782    .       D       .       .       .
1068783    .       .       .       .       .
1068784    .       .       .       .       .
1068785    .       .       D       .       .
1068786    .       .       .       .       .
1068787    .       .       .       D       .
1068788    D       .       .       .       D
1068789    .       .       .       .       .
1068790    .       .       .       .       .
1068791    .       .       .       .       .
1068792    .       .       .       .       .
1068793    .       .       .       .       .
1068794    .       .       .       .       .
1068795    D       D       .       .       .
1068796    .       .       .       .       .
1068797    .       .       .       .       .
~~~

上記の例では、バスID`7`はタイムスタンプ1068788（`t`の7分後）に出発します。
これは問題ありません。
その時刻における唯一の要件はバスID`19`がその時点で出発することで、そうなっています。

他の例を次に示します。

- リスト`17,x,13,19`に適合する最も早いタイムスタンプは`3417`です。
- `67,7,59,61`はタイムスタンプ`754018`に最初に発生します。
- `67,x,7,59,61`はタイムスタンプ`779210`に最初に発生します。
- `67,7,x,59,61`はタイムスタンプ`1261476`に最初に発生します。
- `1789,37,47,1889`はタイムスタンプ`1202161486`に最初に発生します。

ただし、あなたのリストには非常に多くのバスIDがあるため、
実際の最も早いタイムスタンプは確かに`100000000000000`よりも大きくなります！

**リストされたすべてのバスIDが、
リスト内の位置に一致するオフセットで出発するような最も早いタイムスタンプは何ですか？**