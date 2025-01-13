# パート1

これを解くためだけにMD5ハッシュの計算を実装するのはつらいので、ライブラリの力を借りる。
[Wikipedia](https://ja.wikipedia.org/wiki/MD5)を見ると、512ビット=64バイトが処理の単位なので、
この問題の入力はまるまる収まるだろう。

- [cryptohash-md5](https://hackage.haskell.org/package/cryptohash-md5)
- [pureMD5](https://hackage.haskell.org/package/pureMD5)

あたりを使えばよいだろう。
前者を使ってみる。
`hash :: ByteString -> ByteString` で、入力は普通に文字列を `ByteString` 化したもので与えればよいが、
返される結果は文字列ではなく16バイトのバイナリ値で、
問題文のいうところの「`0`が続く」とは`Data.ByteString.Base16.encode` した結果が、という意味である。
5文字の0が続くとは、上位 \\(4 \times 5\\) ビットが0ということで、先頭2バイトは0、
3バイトめは上位4ビットが0なので16未満として判定できるので、`encode` は使わずに済む。

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

# パート2

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
