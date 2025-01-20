2015 day4 もMD5だった。

[pureMD5](https://hackage.haskell.org/package/pureMD5)は `show` で表示形式にできるようなので、こちらを試してみる。


```haskell
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Data.Digest.Pure.MD5
```

```
ghci> md5 $ BSL.fromStrict $ BS.pack "abc3231929"
00000155f8105dff7f56ee10fa9b9abd
```

動いたが、その型変換要るんだ…という気持ち。

```haskell
myinput :: String
myinput = "(あなたのパズル入力)"

part1 :: String -> String
part1 pfx =
    take 8 $ map (!! 5) $ filter (all ('0' ==) . take 5) $
    map (show . md5 . BSL.fromStrict . BS.pack . (pfx ++) . show) [0 ..]
```

```
ghci> part1 "abc"
"18f47a30"
(60.26 secs, 183,796,638,080 bytes)
ghci> part1 myinput
"801b56a7"
(41.49 secs, 160,841,714,424 bytes)
```

なかなかヘビー。

# パート2

ここまでで完成しているパスワードを状態に持ち、次々に送られてくる MD5 digest に対して、

- 先頭5文字が`0`でないならスルー
- 6文字めが0～7でないならスルー
- 6文字めの位置が設定済みならスルー
- 以上のどれでもなければ、7文字めでパスワードを更新

とする `update` 関数を `scanl` で回し、パスワードが未完成な限り捨てる述語 `incomplete` を補助関数に立てる。

途中経過も見たくなったので、更新されたときに表示する `Debug.Trace` も仕掛ける。

```haskell
import Debug.Trace

part2 :: String -> String
part2 pfx =
    head $ dropWhile incomplete $ scanl update "........" $
    map (show . md5 . BSL.fromStrict . BS.pack . (pfx ++) . show) [0 ..]

update res digest
  | any ('0' /=) $ take 5 digest     = res
  | notElem (digest !! 5) ['0'..'7'] = res
  | res !! idx /= '.'                = res
  | otherwise                        = traceShowId $ as ++ (digest !! 6) : bs
  where
    idx = digitToInt (digest !! 5)
    (as,_:bs) = splitAt idx res

incomplete = elem '.'
```

実行結果。本当は Trace とそうでない表示で画面は崩れている。

```
ghci> part2 "abc"
".5......"
".5..e..."
".5..e..3"
".5.ce..3"
"05.ce..3"
"05.ce.e3"
"05.ce8e3"
"05ace8e3"
(98.05 secs, 300,325,387,040 bytes)
ghci> part2 myinput
"4......."
"42......"
"42...1.."
"42...19."
"42...197"
"424..197"
"424.0197"
"424a0197"
(159.67 secs, 541,223,841,200 bytes)
```
