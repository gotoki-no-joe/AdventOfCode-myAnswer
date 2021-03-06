# 18日目：演算の順序 #

窓の外を森林に覆われた大陸が地平線上にゆっくりと現れるのを眺めていると、隣に座っている子供に中断されました。
あなたが彼らの算数の宿題を手伝ってくれるかどうか彼らは興味があります。

残念ながら、この「算数」はあなたが覚えているものとは異なる規則に従っているようです。

宿題（パズルの入力）は、加算(`+`)、乗算(`*`)、括弧(`(...)`)からなる一連の式で構成されています。
通常の数学と同様に、括弧は、周囲の式で使用する前に、内部の式を評価する必要があることを示します。
加算はやはり演算子の両側の数値の合計を検出し、乗算は依然として積を検出します。

ただし、**演算子の優先順位**の規則が変更されました。
加算の前に乗算を評価するのではなく、演算子は**同じ優先順位**を持ち、
現れる順序に関係なく、左から右に評価されます。
(訳注:むしろ現れる順序に従って評価するので、この文は誤り？)

たとえば、式`1 + 2 * 3 + 4 * 5 + 6`を評価する手順は次のとおりです。

~~~
1 + 2 * 3 + 4 * 5 + 6
  3   * 3 + 4 * 5 + 6
      9   + 4 * 5 + 6
         13   * 5 + 6
             65   + 6
                 71
~~~

括弧はこの順序を上書きできます。
たとえば、括弧が追加されて`1 + (2 * 3) + (4 * (5 + 6))`という形になると
どうなるかを次に示します。

~~~
1 + (2 * 3) + (4 * (5 + 6))
1 +    6    + (4 * (5 + 6))
     7      + (4 * (5 + 6))
     7      + (4 *   11   )
     7      +     44
            51
~~~

さらにいくつかの例を示します。

- `2 * 3 + (4 * 5)`は26になります。
- `5 + (8 * 3 + 9 + 3 * 4 * 3)`は`437`になります。
- `5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))`は12240になります。
- `((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2`は13632になります。

宿題を手伝う前に、自分でそれを理解する必要があります。
**宿題の各行の式を評価します。結果の値の合計はいくつですか？**

# パート2 #

あなたはなんとか子供の質問に答え、彼らは宿題の第1部を終えましたが、
次の節「算数**上級編**」に入ったところで行き詰まりました。

今回は、足し算と掛け算は**異なる**優先順位レベルを持っていますが、
それらはあなたが精通しているものではありません。
そうではなく、加算は乗算の**前に**評価されます。

たとえば、式`1 + 2 * 3 + 4 * 5 + 6`を評価する手順は次のとおりです。

~~~
1 + 2 * 3 + 4 * 5 + 6
  3   * 3 + 4 * 5 + 6
  3   *   7   * 5 + 6
  3   *   7   *  11
     21       *  11
         231
~~~

上記の他の例は次のとおりです。

- `1 + (2 * 3) + (4 * (5 + 6))`は今回も51になります。
- `2 * 3 + (4 * 5)`は46になります。
- `5 + (8 * 3 + 9 + 3 * 4 * 3)`は1445になります。
- `5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))`は669060になります。
- `((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2`は23340になります。

**これらの新しい規則を使用して宿題の問題を評価した結果を合計すると、いくつになりますか？**
