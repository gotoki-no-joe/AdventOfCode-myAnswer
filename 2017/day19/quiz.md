# 19日目：一連の管 #

どういうわけか、ネットワークパケットが失われ、ここに行き着きました。
ルーティング図（パズル入力）に従おうとしていますが、どこに行くか混乱しています。

その出発点は、図のすぐ上にあります。（で描かれた行|、-とは+）それが図の上部に接続されている唯一のライン上に下って行くことで始まる、取る必要があるパスを示しています。終わり（図のどこかにあります）に到達するまでこのパスをたどり、そこで停止する必要があります。

時々、線は互いに交差します。このような場合は、同じ方向に進み続ける必要があり、他に選択肢がない場合にのみ左または右に曲がります。さらに、誰かが行に文字を残しました。これらも方向を変えませんが、それを使用して、それがどこにあったかを追跡することができます。例えば：

     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ 

この図を考えると、パケットは次のパスをたどる必要があります。

ダイアグラムの上部に接する唯一の線から開始して、下に移動し、を通過してA、最初の線に進む必要があり+ます。
Bその過程を通り抜けながら、右、上、右に移動します。
下（収集C）、右、上（収集）に進みDます。
最後に、左端まで進みE、で停止しFます。
終わりまでの道をたどると、その道に見える文字はABCDEFです。

小さなパケットがあなたを見上げ、あなたが道を見つけるのを手伝ってくれることを願っています。 パスをたどると、どの文字が（表示される順序で）表示されますか？（ルーティング図は非常に幅が広​​いので、行を折り返さずに表示してください。）

- - パート2 - -
パケットは、いくつのステップを実行する必要があるかを知りたがっています。

たとえば、上記の例と同じルーティング図を使用すると...

     |          
     |  +--+    
     A  |  C    
 F---|--|-E---+ 
     |  |  |  D 
     +B-+  +--+ 

...パケットは行きます：

6 ステップダウンします（図の上部の最初の行を含む）。
3 右のステップ。
4 ステップアップします。
3 右のステップ。
4 降りる。
3 右のステップ。
2 ステップアップします。
13残りのステップ（F停止することを含む）。
これにより、合計38ステップが発生します。

パケットは何ステップ進む必要がありますか？





Somehow, a network packet got lost and ended up here. It's trying to follow a routing diagram (your puzzle input), but it's confused about where to go.

Its starting point is just off the top of the diagram. Lines (drawn with |, -, and +) show the path it needs to take, starting by going down onto the only line connected to the top of the diagram. It needs to follow this path until it reaches the end (located somewhere within the diagram) and stop there.

Sometimes, the lines cross over each other; in these cases, it needs to continue going the same direction, and only turn left or right when there's no other option. In addition, someone has left letters on the line; these also don't change its direction, but it can use them to keep track of where it's been. For example:

     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ 

Given this diagram, the packet needs to take the following path:

Starting at the only line touching the top of the diagram, it must go down, pass through A, and continue onward to the first +.
Travel right, up, and right, passing through B in the process.
Continue down (collecting C), right, and up (collecting D).
Finally, go all the way left through E and stopping at F.
Following the path to the end, the letters it sees on its path are ABCDEF.

The little packet looks up at you, hoping you can help it find the way. What letters will it see (in the order it would see them) if it follows the path? (The routing diagram is very wide; make sure you view it without line wrapping.)

--- Part Two ---
The packet is curious how many steps it needs to go.

For example, using the same routing diagram from the example above...

     |          
     |  +--+    
     A  |  C    
 F---|--|-E---+ 
     |  |  |  D 
     +B-+  +--+ 

...the packet would go:

6 steps down (including the first line at the top of the diagram).
3 steps right.
4 steps up.
3 steps right.
4 steps down.
3 steps right.
2 steps up.
13 steps left (including the F it stops on).
This would result in a total of 38 steps.

How many steps does the packet need to go?
