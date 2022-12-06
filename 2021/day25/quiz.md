コードの出現[約]【イベント】[店][設定][ログアウト]ゴトキの城44 *
       λy。2021年[カレンダー][AoC ++]【スポンサー】【リーダーボード】[統計]
私たちのスポンサーは、コードの出現を可能にするのを助けます：
Honeycomb-あなたはパフォーマンスの良い正しいコードが好きです。私たちもそうします。分散システムは理解しやすいものでなければなりません。Honeycombを無料で使用して、分散システムをデバッグし、無料のシャツを入手してください。ホワイトペーパーをダウンロードして、デモをご覧ください。
--- 25日目：ナマコ---
これがそれです：海溝の底、そりの鍵が最後になる可能性のある場所。あなたの潜水艦の実験用アンテナはまだ鍵を検出するのに十分にブーストされていませんが、それらはここにあるに違いありません。あなたがする必要があるのは海底に到達し、それらを見つけることです。

少なくとも、できれば海底に着陸するでしょう。残念ながら、ナマコの2つの大きな群れで完全に覆われており、潜水艦のための十分な広さのオープンスペースがありません。

潜水艦のコックピットの壁にテープで貼られた手書きのメモで深海の海洋生物学者の電話番号を見つけたので、エルフは以前にこれを行ったに違いないと思われます。

「ナマコ？ええ、彼らはおそらく食べ物を探しています。しかし心配しないでください、彼らは予測可能な生き物です：彼らは完全にまっすぐに動き、そうするスペースがあるときだけ前進します。彼らは実際には非常に礼儀正しいです！ 「」

潜水艦をいつ着陸させることができるかを予測したいと説明します。

「ああ、それは簡単だ。彼らはやがて積み重なって、十分なスペースを残すだろう-待って、潜水艦と言った？そして、ナマコがたくさんいる唯一の場所は、マリアナの一番下にあるだろう-」あなたは電話を切る電話。

同じ地域を共有するナマコの2つの群れがあります。1つは常に東に移動し（>）、もう1つは常に南に移動します（v）。各場所には、最大1つのナマコを含めることができます。残りの場所は空です（.）。潜水艦は、状況のマップ（パズル入力）を作成するのに役立ちます。例えば：

v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>
すべてのステップで、東向きの群れのナマコは1つの場所を前進しようとし、次に南向きの群れのナマコは1つの場所を前進しようとします。群れが前進すると、群れの中のすべてのナマコは、最初に、それが向いている隣接する場所にナマコがあるかどうかを同時に考慮し（同じ方向を向いている別のナマコでさえ）、次に、空の場所に面しているすべてのナマコが同時に移動しますその場所。

したがって、このような状況では：

...>>>>>...
一歩後、一番右のナマコだけが動いたでしょう：

...>>>>.>..
次のステップの後、2つのナマコが移動します。

...>>>.>.>.
1つのステップで、東向きの群れが最初に移動し、次に南向きの群れが移動します。したがって、この状況を考えると：

..........
.>v....v..
.......>..
..........
左側のナマコのうち、1歩後、南向きのナマコだけが移動しました（左側の東向きのナマコが移動するのに間に合わなかったため）が、両方の海右側のきゅうりが移動しました（東向きのナマコが南向きのナマコの邪魔にならないように移動したため）：

..........
.>........
..v....v>.
..........
この地域の強い水流のため、地図の右端から外れるナマコは左端に表示され、地図の下端から外れるナマコは上端に表示されます。ナマコは、目的地が地図の反対側にある場合でも、移動する前に目的地が空かどうかを常に確認します。

Initial state:
...>...
.......
......>
v.....>
......>
.......
..vvv..

After 1 step:
..vv>..
.......
>......
v.....>
>......
.......
....v..

After 2 steps:
....v>.
..vv...
.>.....
......>
v>.....
.......
.......

After 3 steps:
......>
..v.v..
..>v...
>......
..>....
v......
.......

After 4 steps:
>......
..v....
..>.v..
.>.v...
...>...
.......
v......
潜水艦を安全に着陸させる場所を見つけるには、ナマコの動きを止める必要があります。もう一度最初の例を考えてみましょう。

Initial state:
v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>

After 1 step:
....>.>v.>
v.v>.>v.v.
>v>>..>v..
>>v>v>.>.v
.>v.v...v.
v>>.>vvv..
..v...>>..
vv...>>vv.
>.v.v..v.v

After 2 steps:
>.v.v>>..v
v.v.>>vv..
>v>.>.>.v.
>>v>v.>v>.
.>..v....v
.>v>>.v.v.
v....v>v>.
.vv..>>v..
v>.....vv.

After 3 steps:
v>v.v>.>v.
v...>>.v.v
>vv>.>v>..
>>v>v.>.v>
..>....v..
.>.>v>v..v
..v..v>vv>
v.v..>>v..
.v>....v..

After 4 steps:
v>..v.>>..
v.v.>.>.v.
>vv.>>.v>v
>>.>..v>.>
..v>v...v.
..>>.>vv..
>.v.vv>v.v
.....>>vv.
vvv>...v..

After 5 steps:
vv>...>v>.
v.v.v>.>v.
>.v.>.>.>v
>v>.>..v>>
..v>v.v...
..>.>>vvv.
.>...v>v..
..v.v>>v.v
v.v.>...v.

...

After 10 steps:
..>..>>vv.
v.....>>.v
..v.v>>>v>
v>.>v.>>>.
..v>v.vv.v
.v.>>>.v..
v.v..>v>..
..v...>v.>
.vv..v>vv.

...

After 20 steps:
v>.....>>.
>vv>.....v
.>v>v.vv>>
v>>>v.>v.>
....vv>v..
.v.>>>vvv.
..v..>>vv.
v.v...>>.v
..v.....v>

...

After 30 steps:
.vv.v..>>>
v>...v...>
>.v>.>vv.>
>v>.>.>v.>
.>..v.vv..
..v>..>>v.
....v>..>v
v.v...>vv>
v.v...>vvv

...

After 40 steps:
>>v>v..v..
..>>v..vv.
..>>>v.>.v
..>>>>vvv>
v.....>...
v.v...>v>>
>vv.....v>
.>v...v.>v
vvv.v..v.>

...

After 50 steps:
..>>v>vv.v
..v.>>vv..
v.>>v>>v..
..>>>>>vv.
vvv....>vv
..v....>>>
v>.......>
.vv>....v>
.>v.vv.v..

...

After 55 steps:
..>>v>vv..
..v.>>vv..
..>>v>>vv.
..>>>>>vv.
v......>vv
v>v....>>v
vvv...>..>
>vv.....>.
.>v.vv.v..

After 56 steps:
..>>v>vv..
..v.>>vv..
..>>v>>vv.
..>>>>>vv.
v......>vv
v>v....>>v
vvv....>.>
>vv......>
.>v.vv.v..

After 57 steps:
..>>v>vv..
..v.>>vv..
..>>v>>vv.
..>>>>>vv.
v......>vv
v>v....>>v
vvv.....>>
>vv......>
.>v.vv.v..

After 58 steps:
..>>v>vv..
..v.>>vv..
..>>v>>vv.
..>>>>>vv.
v......>vv
v>v....>>v
vvv.....>>
>vv......>
.>v.vv.v..
この例では、ナマコは58ステップの後に動きを停止します。

潜水艦を着陸させるのに安全な場所を見つけてください。ナマコが動かない最初のステップは何ですか？

あなたのパズルの答えはでした384。

このパズルの前半は完了です！それは1つの金の星を提供します：*

- - パート2 - -
突然、実験用アンテナ制御コンソールが点灯します。

Sleigh keys detected!
コンソールによると、キーは潜水艦の真下にあります。あなたは彼らに正しく着陸しました！潜水艦のロボットアームを使用して、そりの鍵をエアロックに移動します。

今、あなたはクリスマスを救うためにそれらをサンタに間に合わせる必要があります！あなたはあなたの時計をチェックします-それはクリスマスです。時間内にそれらを表面に戻すことができる方法はありません。

希望を失い始めると、そりキーのボタンに気づきます：リモートスタート。海底からそりを始めることができます！キーからの信号をブーストして、実際にそりに到達するようにする方法が必要です。潜水艦がその実験的なアンテナを持っているのは良いことです！ただし、これまでにブーストするには、間違いなく50個の星が必要です。

実験用アンテナ制御コンソールが再び点灯します。

Energy source detected.
Integrating energy source from device "sleigh keys"...done.
Installing device drivers...done.
Recalibrating experimental antenna...done.
Boost strength due to matching signal phase: 1 star
残り49個の星。

ただし、信号をブーストするのに十分な星がありません。さらに5つ必要です。

変更はありませんが、パズルの入力を取得できます。

あなたは[共有することができます]このパズル。

Advent of Code[About][Events][Shop][Settings][Log Out]gotoki-no-joe 44*
       λy.2021[Calendar][AoC++][Sponsors][Leaderboard][Stats]
Our sponsors help make Advent of Code possible:
Honeycomb - You like performant, correct code. So do we. Distributed systems should be easy to understand. Use Honeycomb for free to debug your distributed systems and get a free shirt. Download our white papers and watch our demo.
--- Day 25: Sea Cucumber ---
This is it: the bottom of the ocean trench, the last place the sleigh keys could be. Your submarine's experimental antenna still isn't boosted enough to detect the keys, but they must be here. All you need to do is reach the seafloor and find them.

At least, you'd touch down on the seafloor if you could; unfortunately, it's completely covered by two large herds of sea cucumbers, and there isn't an open space large enough for your submarine.

You suspect that the Elves must have done this before, because just then you discover the phone number of a deep-sea marine biologist on a handwritten note taped to the wall of the submarine's cockpit.

"Sea cucumbers? Yeah, they're probably hunting for food. But don't worry, they're predictable critters: they move in perfectly straight lines, only moving forward when there's space to do so. They're actually quite polite!"

You explain that you'd like to predict when you could land your submarine.

"Oh that's easy, they'll eventually pile up and leave enough space for-- wait, did you say submarine? And the only place with that many sea cucumbers would be at the very bottom of the Mariana--" You hang up the phone.

There are two herds of sea cucumbers sharing the same region; one always moves east (>), while the other always moves south (v). Each location can contain at most one sea cucumber; the remaining locations are empty (.). The submarine helpfully generates a map of the situation (your puzzle input). For example:

v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>
Every step, the sea cucumbers in the east-facing herd attempt to move forward one location, then the sea cucumbers in the south-facing herd attempt to move forward one location. When a herd moves forward, every sea cucumber in the herd first simultaneously considers whether there is a sea cucumber in the adjacent location it's facing (even another sea cucumber facing the same direction), and then every sea cucumber facing an empty location simultaneously moves into that location.

So, in a situation like this:

...>>>>>...
After one step, only the rightmost sea cucumber would have moved:

...>>>>.>..
After the next step, two sea cucumbers move:

...>>>.>.>.
During a single step, the east-facing herd moves first, then the south-facing herd moves. So, given this situation:

..........
.>v....v..
.......>..
..........
After a single step, of the sea cucumbers on the left, only the south-facing sea cucumber has moved (as it wasn't out of the way in time for the east-facing cucumber on the left to move), but both sea cucumbers on the right have moved (as the east-facing sea cucumber moved out of the way of the south-facing sea cucumber):

..........
.>........
..v....v>.
..........
Due to strong water currents in the area, sea cucumbers that move off the right edge of the map appear on the left edge, and sea cucumbers that move off the bottom edge of the map appear on the top edge. Sea cucumbers always check whether their destination location is empty before moving, even if that destination is on the opposite side of the map:

Initial state:
...>...
.......
......>
v.....>
......>
.......
..vvv..

After 1 step:
..vv>..
.......
>......
v.....>
>......
.......
....v..

After 2 steps:
....v>.
..vv...
.>.....
......>
v>.....
.......
.......

After 3 steps:
......>
..v.v..
..>v...
>......
..>....
v......
.......

After 4 steps:
>......
..v....
..>.v..
.>.v...
...>...
.......
v......
To find a safe place to land your submarine, the sea cucumbers need to stop moving. Again consider the first example:

Initial state:
v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>

After 1 step:
....>.>v.>
v.v>.>v.v.
>v>>..>v..
>>v>v>.>.v
.>v.v...v.
v>>.>vvv..
..v...>>..
vv...>>vv.
>.v.v..v.v

After 2 steps:
>.v.v>>..v
v.v.>>vv..
>v>.>.>.v.
>>v>v.>v>.
.>..v....v
.>v>>.v.v.
v....v>v>.
.vv..>>v..
v>.....vv.

After 3 steps:
v>v.v>.>v.
v...>>.v.v
>vv>.>v>..
>>v>v.>.v>
..>....v..
.>.>v>v..v
..v..v>vv>
v.v..>>v..
.v>....v..

After 4 steps:
v>..v.>>..
v.v.>.>.v.
>vv.>>.v>v
>>.>..v>.>
..v>v...v.
..>>.>vv..
>.v.vv>v.v
.....>>vv.
vvv>...v..

After 5 steps:
vv>...>v>.
v.v.v>.>v.
>.v.>.>.>v
>v>.>..v>>
..v>v.v...
..>.>>vvv.
.>...v>v..
..v.v>>v.v
v.v.>...v.

...

After 10 steps:
..>..>>vv.
v.....>>.v
..v.v>>>v>
v>.>v.>>>.
..v>v.vv.v
.v.>>>.v..
v.v..>v>..
..v...>v.>
.vv..v>vv.

...

After 20 steps:
v>.....>>.
>vv>.....v
.>v>v.vv>>
v>>>v.>v.>
....vv>v..
.v.>>>vvv.
..v..>>vv.
v.v...>>.v
..v.....v>

...

After 30 steps:
.vv.v..>>>
v>...v...>
>.v>.>vv.>
>v>.>.>v.>
.>..v.vv..
..v>..>>v.
....v>..>v
v.v...>vv>
v.v...>vvv

...

After 40 steps:
>>v>v..v..
..>>v..vv.
..>>>v.>.v
..>>>>vvv>
v.....>...
v.v...>v>>
>vv.....v>
.>v...v.>v
vvv.v..v.>

...

After 50 steps:
..>>v>vv.v
..v.>>vv..
v.>>v>>v..
..>>>>>vv.
vvv....>vv
..v....>>>
v>.......>
.vv>....v>
.>v.vv.v..

...

After 55 steps:
..>>v>vv..
..v.>>vv..
..>>v>>vv.
..>>>>>vv.
v......>vv
v>v....>>v
vvv...>..>
>vv.....>.
.>v.vv.v..

After 56 steps:
..>>v>vv..
..v.>>vv..
..>>v>>vv.
..>>>>>vv.
v......>vv
v>v....>>v
vvv....>.>
>vv......>
.>v.vv.v..

After 57 steps:
..>>v>vv..
..v.>>vv..
..>>v>>vv.
..>>>>>vv.
v......>vv
v>v....>>v
vvv.....>>
>vv......>
.>v.vv.v..

After 58 steps:
..>>v>vv..
..v.>>vv..
..>>v>>vv.
..>>>>>vv.
v......>vv
v>v....>>v
vvv.....>>
>vv......>
.>v.vv.v..
In this example, the sea cucumbers stop moving after 58 steps.

Find somewhere safe to land your submarine. What is the first step on which no sea cucumbers move?

Your puzzle answer was 384.

The first half of this puzzle is complete! It provides one gold star: *

--- Part Two ---
Suddenly, the experimental antenna control console lights up:

Sleigh keys detected!
According to the console, the keys are directly under the submarine. You landed right on them! Using a robotic arm on the submarine, you move the sleigh keys into the airlock.

Now, you just need to get them to Santa in time to save Christmas! You check your clock - it is Christmas. There's no way you can get them back to the surface in time.

Just as you start to lose hope, you notice a button on the sleigh keys: remote start. You can start the sleigh from the bottom of the ocean! You just need some way to boost the signal from the keys so it actually reaches the sleigh. Good thing the submarine has that experimental antenna! You'll definitely need 50 stars to boost it that far, though.

The experimental antenna control console lights up again:

Energy source detected.
Integrating energy source from device "sleigh keys"...done.
Installing device drivers...done.
Recalibrating experimental antenna...done.
Boost strength due to matching signal phase: 1 star
Only 49 stars to go.

You don't have enough stars to boost the signal, though. You need 5 more.

Although it hasn't changed, you can still get your puzzle input.

You can [Share] this puzzle.

