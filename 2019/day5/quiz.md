# 5日目：晴れ時々流星群 #

船が水星に向かって進むにつれて、汗が出てきました。
妖精は、熱環境監視端末をサポートするように船内のコンピュータをアップグレードして、エアコンを作動させることを提案してきました。

温度環境監視端末(TEST)は*診断プログラム*（パズル入力）を実行することにより起動します。
TEST診断プログラムは、[既存のIntcodeコンピュータ](../day2/quiz.md)にいくつか変更をすると実行できます。

最初に、2つの新しい*命令*を追加する必要があります。

- オペコード3は、単一の整数を*入力*として受け取り、その唯一のオペランドで指定されたアドレスに保存します。
例えば命令`3,50`は入力値を取り、それをアドレス50に保存します。
- オペコード4は、その唯一のオペランドの値を*出力*します。
例えば命令`4,50`はアドレス50の値を出力します。

これらの命令を使用するプログラムには、入力と出力に何を接続する必要があるかを説明するドキュメントが付属しています。
プログラム`3,0,4,0,99`は、入力として取得したものを何であれ出力し、停止します。

*次に*、*アドレッシングモード*の対応を追加する必要があります。

命令の各オペランドはアドレッシングモードに基づいて処理されます。
現時点で、船のコンピュータは既にアドレッシングモード0の*位置モード*を理解しています。
これはオペランドを*位置*として解釈します。
オペランドが50ならば、その値は*メモリ内のアドレス50に格納されている値*です。
これまでは全てのオペランドは位置モードでした。

ここから、船のコンピュータはモード1、*即値モード*でもオペランドを処理する必要があります。
即値モードではオペランドは*値*として解釈されます。
オペランドが50の場合、その値は単に*50*です。

アドレッシングは命令のオペコードと同じ値に保存されます。
オペコードは値の1の位と10の位のみに基づく2桁の数値です。
つまり、オペコードは命令の最初の値の右端2桁です。
アドレッシングモードは単一の数字で、オペランドごとに1桁で、オペコードから右から左に読み取ります。
1つめのオペランドのモードは百の位、2つめのオペランドのモードは千の位、3つめのオペランドのモードは1万の位、以下同様です。
欠落しているモードは全て0とみなします。

例えばプログラム`1002,4,3,4,33`を考えます。

最初の命令`1002,4,3,4`は乗算命令です。
最初の値の右端2桁`02`は、オペコード2、乗算を示します。
次に、右から左に進むと、アドレッシングは0（百の位）、1（千の位）、および0（1万の位、存在しないため0）です。

```
ABCDE
 1002

DE - 2桁のオペコード,       02 == オペコード 2
 C - 第1オペランドのモード,  0 == 位置モード
 B - 第2オペランドのモード,  1 == 即値モード
 A - 第3オペランドのモード,  0 == 位置モード（0で補完）
```

この命令は、最初の2つのオペランドを乗算します。
位置モードの最初のオペランドは以前と同じように動作します。
その値はアドレス4に格納されている値(33)です。
即値モードの2番目のオペランドは単に値3を持ちます。
この操作の結果33 * 3 = 99は、位置モードの3番目のオペランド4に従って書き込まれます。
これは以前と同じように動作し、99はアドレス4に書き込まれます。

命令が書き込むオペランドは*即時モードには決してなりません*。

*最後に*、いくつかのメモ：

- 命令の完了後、命令ポインタは*命令の値の数だけ*増加することに注意してください。
新たな命令により、この量は常に一定の値4ではもはやありません。
- 整数には負の値を指定できます。
`1101,100,-1,4,0`は有効なプログラムです。
（100 + -1を計算して結果を位置4に保存します。）

TEST診断プログラムは、*入力*命令を実行してユーザーからテストするシステムのIDを要求することから始まります。
これには、船のエアコンユニットのIDである1を与えてください。

その後、一連の診断テストを実行して、オペランドモードなどのIntcodeコンピュータのさまざまな部分が正しく機能することを確認します。
各テストについて、テストの結果が期待値からどれだけ離れているかを示す*出力*命令を実行します。
ここで0はテストが成功したことを意味します。
非零の出力は機能が正しく動作していないことを意味します。
出力命令の前に実行された命令をチェックして、失敗した命令を確認します。

最後に、プログラムは*診断コード*を出力し、すぐに停止します。
この最終出力はエラーではありません。
出力の直後の停止はプログラムが終了したことを意味します。
診断コードを除く全ての出力が零の場合、診断プログラムは正常に実行されました。

唯一の入力命令に1を与え、全てのテストに合格した後、*プログラムが生成する診断コードは何ですか？*

# パート2 #

エアコンがオンラインになります！
冷たい空気はしばらく心地よく感じますが、その後TESTアラームが鳴り始めます。
エアコンはその熱をどこにも放出する先はなく宇宙船に戻すことしかできないため、実際には船内の空気を*暖めて*います。

代わりに、TESTを使用して宇宙船のラジエーターを展開する必要があります。
幸いなことに、このための診断プログラム（パズル入力）が既に装備されています。
残念ながら、あなたのIntcodeコンピュータの準備ができていません。

あなたのコンピューターには以下のいくつかのオペコードがありません。

- オペコード5は`jump-if-true`です。
第1オペランドが*非零*のとき、命令ポインタを第2オペランドの値に設定します。
そうでなければ何もしません。
- オペコード6は`jump-if-false`です。
第1オペランドが*零*のとき、命令ポインタを第2オペランドの値に設定します。
そうでなければ何もしません。
- オペコード7は`less than`です。
第1オペランドが第2オペランド*より小さい*とき、第3オペランドで指定された位置に1が保存されます。
それ以外の場合は0を保存します。
- オペコード8は`equals`です。
第1オペランドが第2オペランドと*等しい*とき、第3オペランドで指定された位置に1が保存されます。
それ以外の場合は0を保存します。

全ての命令と同様に、これらの命令は上記の*アドレッシングモード*に対応する必要があります。

通常、命令が終了すると、その命令の値の数だけ命令ポインタが増加します。
*しかし*、命令が命令ポインタを変更する場合、その値が使用され、命令ポインタは*自動的に増加しません*。

例として、ひとつの入力を受け取り、それを値8と比較してからひとつの出力を生成するプログラムをいくつか示します。

- `3,9,8,9,10,9,4,9,99,-1,8`：
*位置モード*を使用して、入力が8と*等しい*かどうかを調べます。
出力`1`（そうである場合）または`0`（そうでない場合）
- `3,9,7,9,10,9,4,9,99,-1,8`：
*位置モード*を使用して、入力が8*より小さい*かどうかを調べます。
出力`1`（そうである場合）または`0`（そうでない場合）
- `3,3,1108,-1,8,3,4,3,99`：
*即時モード*を使用して、入力が8と*等しい*かどうかを調べます。
出力`1`（そうである場合）または`0`（そうでない場合）
- `3,3,1107,-1,8,3,4,3,99`：
*即時モード*を使用して、入力が8未満かどうかを調べます。
出力`1`（そうである場合）または`0`（そうでない場合）

入力をひとつとり、その入力が零ゼロのとき`0`、または入力が零以外のとき`1`を出力するジャンプテストを次に示します。

- `3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9`（*位置モード*を使用）
- `3,3,1105,-1,9,1101,0,0,12,4,12,99,1`（*即値モード*を使用）

より大きな例を次に示します。

```
3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
```

上のプログラム例では、入力命令を使用して数をひとつ要求しています。
プログラムは入力値が8未満のとき`999`を、
入力値が8に等しいとき`1000`を、
入力値が8より大きいとき`1001`を出力します。

今回は、TEST診断プログラムがテストするシステムのIDを取得するために入力命令を実行したとき、
船のサーマルラジエーターコントローラのIDである`5`をそれに提供します。
この診断テストスイートは*診断コード*という1つの数値のみを出力します。

*システムID 5の診断コードはいくつですか？*
