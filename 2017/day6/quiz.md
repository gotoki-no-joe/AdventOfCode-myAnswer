# 6日目：メモリの再割り当て #

このデバッガプログラムに問題があります。
メモリの再配置ルーチンを修復しようとしていますが、
無限ループに陥っています。

この領域には16のメモリバンクがあります。
各メモリバンクは任意の数の**ブロック**を保持することができます。
再割り当てルーチンの目的は、メモリバンク間のブロックのバランスをとることです。

再配置ルーチンは1クロックサイクルで動作します。(?)
各サイクルで、最もブロック数の多いメモリバンク
（同着のときは最も小さい番号のメモリバンクを選ぶ）を見つけ、
それらのブロックをバンク間で再分配する。
これを行うには、
選択したバンクからすべてのブロックを削除し、
（インデックス順で）次のメモリバンクに移動し、
ブロックの1つを挿入します。
ブロックがなくなるまでこれを続けます。
最後のメモリバンクに到達したときは、最初のバンクに戻って続けます。

デバッガは、
**既に遭遇したことのある**バンク中のブロック構成が生成されるまでに、
何度いくつの再配置を行うことができるかを知りたい。

たとえば、4つのメモリバンクしかないシナリオを想像してみてください。

- バンクはそれぞれ0, 2, 7, 0ブロックで始まります。
3番目のバンクはブロックが最も多いので、再分配のために選択されます。
- 次のバンク（第4のバンク）から始めて、第1のバンク、第2のバンク…と続き、
7つのブロックはメモリバンクに分配されます。
第4、第1、第2のバンクはそれぞれ2つのブロックを得て、
第3のバンクは1つ取り戻します。
最終的な結果は2 4 1 2のようになります。
- 次に、2番目のバンクが選択されます。
これは、最も多くのブロック（4個）を持つためです。
メモリバンクは4つあるため、それぞれが1つのブロックを取得します。
結果は3 1 2 3となります。
- 今、第1と第4のメモリバンクはどちらも3つのブロックを持って同着となります。
第1のバンクが選択され、
その3つのブロックは他の3つのバンクに均等に配分されます。0 2 3 4となります。
- 第4のバンクが選択され、
その4つのブロックは、4つのバンクそれぞれが1つを受け取るように分配されます。
1 3 4 1となります。
- 3番目のバンクが選択され、同様にして2 4 1 2となります。

この時点で、私たちはこれまでに見たことのある状態に遭遇しました。
2 4 1 2は既に見たことがあります。
5回目のブロックの再配布サイクルの後に
無限ループが検出されるため、この例の答えは`5`です。

あなたのパズル入力にある初期ブロック数を考えて、
既出の構成が作成されるまでに**何回再配布サイクル**を完了する必要がありますか？

# パート2 #

好奇心から、
デバッガはループのサイズも知りたいと思うでしょう。
すでに見た状態から始めて、
同じ状態が再び見られる前に何回ブロックの再配布サイクルを実行する必要がありますか？

上記の例で2 4 1 2は4サイクル後に再び表示されるため、
その例の回答は4となります。

あなたのパズル入力の設定から生じる無限ループの**サイクル数**は何回ですか？