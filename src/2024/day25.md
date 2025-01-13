# 25日目: AoC年代記

アイデアも時間も尽きた歴史学者たちは、念のために**主任歴史学者**のオフィスをもう一度確認しに戻るべきだと合意しました。
彼があなたに気づかれずにそこに戻った可能性があるからです。

そこに着くと、彼のオフィスのドアが**施錠**されていることに驚きます！
中に誰かがいるのが聞こえますが、ノックしても反応はありません。
この階の錠前は全て豪華で高価な、[5ピンのシリンダー錠](https://ja.wikipedia.org/wiki/%E3%83%94%E3%83%B3%E3%82%BF%E3%83%B3%E3%83%96%E3%83%A9%E3%83%BC%E9%8C%A0)のバーチャル版なので、
ドアを開ける手助けが頼めるか、北極のセキュリティに連絡します。

残念ながら彼らは、どの錠が取り付けられていて、どの鍵がそれに対応しているのかを見失ってしまっているため、
彼らができる最善のことは、あなたがいるフロアの**全ての錠と全ての鍵の図面**（あなたのパズル入力）を送ることです。

図面は暗号化されたファイル形式ですが、製造元の情報が含まれているため、あなたはメーカーサポートの電話番号を調べます。

「弊社のバーチャル5ピンシリンダー製品ですか？
それは弊社の最も高価なモデルです！**どこよりも**安全でー」
あなたはドアを開ける必要があり、あまり時間がないことを説明します。

「まあ、鍵が錠を開けるかどうかは、実際に鍵を錠に試さなければわかりません（量子隠れ変数のため）、
しかしいくつかの鍵と錠の組み合わせを除外することは**できます**。」

「仮想システムは複雑ですが、その一部は実際には主にマーケティング上の理由から、
5ピンシリンダー錠の粗いシミュレーションです。
図面を見れば、鍵が錠に合う可能性があるかどうかを判断できます。」

彼はあなたにいくつかの図面の例を送信します：

```
#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####
```

「錠の図面は、最上行が埋まっており（`#`）と最下行が空いている（`.`）ものです。
鍵の図面は、最上行が空いていて最下行が埋まっています。
よく見ると、各図面は実際にはさまざまな高さの列の集まりであり、
錠の場合は上から下に、鍵の場合は下から上に伸びています。」

「錠の場合、それらはピンそのものです。図面のピンを列ごとの高さのリストに変換できます。
鍵の場合、列はピンと整列する鍵の形を成しています。それらも高さのリストに変換できます。」

「したがって、最初の錠はピンの高さが `0,5,3,4,3` であると言えます。」

```
#####
.####
.####
.####
.#.#.
.#...
.....
```

「また、最初の鍵は高さは `5,0,2,1,3` です。」

```
.....
#....
#....
#...#
#.#.#
#.###
#####
```

「これらは一緒に合うように見えます。
最初の4つの列では、ピンと鍵は重なりません。しかし、この鍵はこの錠には**合いません**。
最も右の列では、錠のピンが鍵と重なっており、その列では錠の高さと鍵の高さの合計が
空間の大きさを超えていることがわかります。」

「とにかく、それぞれの鍵を錠それぞれでテストすることで、試すべき鍵を絞り込むことができます。
つまり、チェックしなければならないのは…
待って、錠は**いくつ**あるのですか？
でも、**その**サイズのものを設置したのは一件だけ、ほっきょー」
あなたは通話を切ります。

この例では、両方の錠をピンの高さに変換すると、次のようになります：

```
0,5,3,4,3
1,2,0,5,3
```

3つの鍵全てを高さに変換すると、次のようになります：

```
5,0,2,1,3
4,3,4,0,2
3,0,2,0,1
```

そして、全ての鍵を全ての錠に試すことができます：

- 錠 `0,5,3,4,3` と鍵 `5,0,2,1,3` : 最後の列で**重なり**があります。
- 錠 `0,5,3,4,3` と鍵 `4,3,4,0,2` : 2番目の列で**重なり**があります。
- 錠 `0,5,3,4,3` と鍵 `3,0,2,0,1` : 全ての列が**合います**！
- 錠 `1,2,0,5,3` と鍵 `5,0,2,1,3` : 最初の列で**重なり**があります。
- 錠 `1,2,0,5,3` と鍵 `4,3,4,0,2` : 全ての列が**合います**！
- 錠 `1,2,0,5,3` と鍵 `3,0,2,0,1` : 全ての列が**合います**！

この例では、どの列でも重ならずに合う錠と鍵の組み合わせの数は3です。

あなたの錠と鍵の図面を分析してください。
どの列でも重ならずに合う錠と鍵の組み合わせはいくつありますか？

# パート2

あなたと歴史学者たちはオフィスに押し入り、主任歴史学者は驚いて目を覚まします！
歴史学者たちは皆、順番に混乱した表情を浮かべ、一人が主任はここ数ヶ月どこにいたのかと尋ねます。

「私はずっとここにいて、サンタからのこの優先度の高いリクエストに取り組んでいました！
私が離れたのは、約1ヶ月前にコーヒーを取りに行ったときだけだと思います…」

その時、主任は時間に気づきます。
「ああ、いけない！遅れそうだ！サンタがリクエストしたこの年代記の仕上げをしようとして寝てしまったに違いない。
サンタが出発する前にリストの残り50か所を訪れて、年代記を完成させる時間が足りない！
サンタは今夜のそりの出発前にこれ必要だと言っていました。」

歴史学者の一人が、これまでずっと使ってきた、どこを探していたかを記録していたリストを掲げます。
皆が訪れた各場所の横には、その場所に**スター**が付けられています。
他の歴史学者たちも旅の間に取った自分のメモを掲げています。
歴史学者として、そんな歴史的に重要な場所を訪れながら、全てを書き留めずにはいられなかったのでしょう。

主任の目が大きく開きます。
「これだけあれば、年代記を完成させるのに時間は十分にあるかもしれません！
サンタはそれをリボンで包んでほしいと言っていたから、ラッピング部門に連絡します…
ああ、それをサンタのところに持って行ってくれませんか？
その頃にはそりの出発を見るために自分の席にいないとならないのです。」

あなたはうなずき、歴史学者たちは急いで年代記の最終ページのためにメモを集めます。

## 足らない場合

しかし、年代記を完成させるための星が足りません。あと**個必要です。

## 足りた場合

年代記を届けるのに十分な星があります。

（リンクを進むと）

**50個のスター**で示された場所のメモを使って、歴史学者たちは年代記を完成させ、それを包んで、
そりの大発射の前にサンタに届けるようにあなたに渡します。

あなたが到着したとき、サンタはそりの中で最終的な出発準備をしているところでした。
あなたは彼に年代記を渡そうとしますが、彼はそれを受け取りません。
「ホー、ホー、ホー」と彼は自分に笑います。
「その贈り物は私のためではなく、**あなた**のものです。
その年代記は、あなたが過去10年間に訪れた場所や助けた人々の記録です。すべてに感謝します。」
そう言って、サンタは今年の残りのプレゼントを配るためにそりを飛ばします。

（また、興味があれば[ショップ](https://cottonbureau.com/people/advent-of-code)に新しいデザインがアップされています。
見えない場合は、少し待ってみてください。）

おめでとうございます！あなたは2024年のAdvent of Codeの全てのパズルを解き終えました！
私があなたのためにそれらを作るのと同じくらい、解くのが楽しかったことを願っています。<!-- はい、とっても。-->
あなたの冒険について聞かせていただけると嬉しいです。
私の[ウェブサイト](https://was.tl/)の連絡先情報や[Bluesky](https://bsky.app/profile/was.tl),
[Mastodon](https://hachyderm.io/@ericwastl), [Twitter](https://twitter.com/ericwastl)を通じてご連絡いただけます。

将来的にこのようなものをもっと見たい場合は、Advent of Codeを[サポート](https://adventofcode.com/2024/support)し、他の人と共有することを検討してください。

各パズルのイースターエッグを<u>ハイライト</u>しましたので、見逃した場合はご確認ください。
それらにマウスを重ねると、イースターエッグが表示されます。