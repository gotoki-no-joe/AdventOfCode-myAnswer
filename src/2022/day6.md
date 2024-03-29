# 6 日目: チューニングのトラブル

準備がようやく完了しました。
あなたは小人たちと徒歩でキャンプを出発し、スターフルーツの果樹園に向けて進み始めます。

密集した下草の中を移動していると、小人の1人が携帯**装置**を渡してくれます。
彼はそれが多くの素晴らしい機能を持っていると言いますが、今セットアップする最も重要なものは**通信システム**です.

しかし、あなたは信号ベースのシステムを扱った経験が豊富
（[2016/6][2016/25][2019/7][2019/9][2019/16][2021/25]）
だと聞いたので、
彼は他の小人に、彼らの故障したデバイスを1つ渡しても大丈夫だと確信させました。
もちろんあなたは簡単にそれを直せるでしょう。

コントのようなタイミングで、その装置はカラフルな火花をいくらか放ちます。

小人と通信できるようにするには、**装置が小人の信号にロックオン**する必要があります。
信号は、装置が一度に1つずつ受信する、一見ランダムな文字列です。

通信システムを修正するには、データストリームから**パケット開始マーカー**を検出する
サブルーチンをデバイスに追加する必要があります。
小人が使用しているプロトコルでは、パケットの開始は、**全て異なる4文字の列**によって示されます。

装置はサブルーチンにデータストリームバッファ（パズル入力）を送信します。
サブルーチンは、受信した直近4文字が全て異なっている最初の位置を特定する必要があります。
具体的には、バッファの先頭から最初の4文字マーカーの末尾までの文字数を報告する必要があります。

例えば、次のデータストリーム バッファを受け取ったとします：

```
mjqjpqmgbljsphdztnvjfqwrcgsmlb
```

最初の3文字 (`mjq`) が受信された後、マーカーを見つけるのに十分な文字がまだ受信されていません。
最初にマーカーが発生する可能性があるのは4文字めが受信された後で、
すると最新の4文字は `mjqj` となります。
`j` が繰り返されているため、これはマーカーではありません。

最初にマーカーが現れるるのは**7文字め**が到着した後です。
そのとき、直近で受け取った4文字は `jpqm` であり、これは全てが異なります。
この場合、7文字が処理された後に最初のパケット開始マーカーが完成するため、
サブルーチンは値7を報告する必要があります。

次にいくつかの例を示します。

- `bvwbjplbgvbhsrlpgdmjqwftvncz` : 最初のマーカーは5文字後
- `nppdvjthqldpwncqszvftbrmjlhg` : 最初のマーカーは6文字後
- `nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg` : 最初のマーカーは10文字後
- `zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw` : 最初のマーカーは11文字後

**最初のパケット開始マーカーが検出されるまでに、何文字を処理する必要がありますか？**

<!--
<details><summary>解説</summary><div>

全ての位置から開始した4文字の並びを作るには、`Data.List.Split.divvy` も使えるが、
`map (take 4) . tails` でできる。

これを `nub` しても4文字のままであるものの先頭がいくつめかを数え、文字の長さ分補正する。

なぜか、マーカーの長さをパラメータ化しておいてみる。

```haskell
main1 = readFile "input.txt" >>= print . detect 4

detect :: Int -> String -> Int
detect k = (k +) . length . takeWhile id . map ((k >) . length . nub . take k) . tails
```

</div></details>
-->

# パート2

装置の通信システムはパケットを正しく検出していますが、まだ正しく動きません。
**メッセージ**も探す必要があるようです。

**メッセージ開始マーカー**は、パケット開始マーカーと同じですが、4文字ではなく**異なる14文字**で構成される点が異なります。

上記の全ての例のメッセージ開始マーカーの最初の位置は次のとおりです。

- `mjqjpqmgbljsphdztnvjfqwrcgsmlb` : 最初のマーカーは19文字後
- `bvwbjplbgvbhsrlpgdmjqwftvncz` : 最初のマーカーは23文字後
- `nppdvjthqldpwncqszvftbrmjlhg` : 最初のマーカーは23文字後
- `nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg` : 最初のマーカーは29文字後
- `zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw` : 最初のマーカーは26文字後

**最初のメッセージ開始マーカーが検出されるまでに、何文字を処理する必要がありますか？**

<!--
<details><summary>解説</summary><div>

パラメータ化しておいたので、それを変更するだけ。

```haskell
main2 = readFile "input.txt" >>= print . detect 14
```

</div></details>
-->
