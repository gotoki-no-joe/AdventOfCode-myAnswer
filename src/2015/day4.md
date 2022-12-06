# 4日目：理想的な長靴下の詰め合わせ

（原題は stocking stuffer で頭韻を踏んでいる）

サンタは
AdventCoins（**bitcoins**と非常によく似た何か）を**マイニング**する手助けを求めています。
経済的に先進的な全ての子供たちへのプレゼントにするのです。

それには、**MD5**ハッシュが**16進数**で少なくとも**5つのゼロ**で始まるようなものを見つける必要があります。
MD5ハッシュへの入力は何かの秘密鍵（パズル入力）に10進数の数字を続けたものです。
AdventCoinsをマイニングするために、
そのようなハッシュを生成するような最小の正の数（先行する零は付けずに1,2,3,…で）を見つける必要があります。

例えば：

- あなたの秘密鍵が`abcdef`のとき、その答は`609043`です。
というのは`abcdef609043`のMD5のハッシュは5つの零で始まり（`000001dbbfa...`）、またそうなる最小の数だからです。
- あなたの秘密鍵が`pqrstuv`のとき、連結して5つの零で始まるMD5ハッシュを作る最小の数は`1048970`です。
つまり、`pqrstuv1048970`のMD5ハッシュは`000006136ef...`となります。

あなたのパズル入力は`iwrupvqb`です。

<details><summary>解説</summary><div>

これを解くためだけにMD5ハッシュの計算を実装するのはつらいので、ライブラリの力を借りる。
[Wikipedia](https://ja.wikipedia.org/wiki/MD5)を見ると、512ビット=64バイトが処理の単位なので、
この問題の入力はまるまる収まるだろう。

- [cryptohash-md5](https://hackage.haskell.org/package/cryptohash-md5)
- [pureMD5](https://hackage.haskell.org/package/pureMD5)

あたりを使えばよいだろう。

```haskell
import qualified Data.ByteString.Char8 as BS
import qualified Crypto.Hash.MD5 as MD5

part1 :: String  -- 秘密鍵
      -> Int     -- 答え
part1 sk = head
  [ i
  | i <- [1..]
  , let digest = MD5.hash $ BS.pack $ sk ++ show i
  , fromEnum (BS.index digest 0) ==  0
  , fromEnum (BS.index digest 1) ==  0
  , fromEnum (BS.index digest 2) <  16
  ]
```

</div></details>


# パート2

今度は6つの0で始まるようなものを見つけてください。

<details><summary>解説</summary><div>

パート1と変わらない。

```haskell
part2 :: String  -- 秘密鍵
      -> Int     -- 答え
part2 sk = head
  [ i
  | i <- [1..]
  , let digest = MD5.hash $ BS.pack $ sk ++ show i
  , fromEnum (BS.index digest 0) == 0
  , fromEnum (BS.index digest 1) == 0
  , fromEnum (BS.index digest 2) == 0
  ]
```

</div></details>
