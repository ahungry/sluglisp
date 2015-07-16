PERCENT-ENCODING
================

What is this?
-------------

Percent-encoding is a library for
[percent-encoding](http://tools.ietf.org/html/rfc3986#section-2.1) defined in
[RFC 3986](http://tools.ietf.org/html/rfc3986) and varieties.

[RFC 3986](http://tools.ietf.org/html/rfc3986)で定義されている
[パーセントエンコーディング](http://tools.ietf.org/html/rfc3986#section-2.1)とそ
の亜種を処理するためのライブラリです。

License
-------

It's licensed under the MIT license.

Requirements
------------

* [Anaphora](http://common-lisp.net/project/anaphora/)
* [Babel](http://common-lisp.net/project/babel/)

API
---

### Encoding And Decoding

#### Function: encode string &key test www-form encoding

Encodes `string` according to percent-encoding and returns it as a string.

`string`をパーセントエンコーディングに従ってエンコードし、文字列として返します。

`test` is a function of one argument that returns a generalized boolean. It's
used for determining whether each octet requires encoding. If it returns
**false**, the octet is encoded. The default value is `#'unreservedp`.

`test`はひとつの引数を持ち、真偽値を返す関数です。それぞれのオクテットにエンコー
ディングが必要かどうかを決めるために使われます。**偽**を返した場合、そのオクテッ
トはエンコードされます。標準の値は`#'unreservedp`です。

If `www-form` is true, returns an application/x-www-form-urlencoded string instead
of an RFC 3986 percent-encoded string.

`www-form`が真の場合、RFC 3986のパーセントエンコーディングの代わりに
application/x-www-form-urlencodedを使ってエンコードした文字列を返します。

`encoding` is a character encoding scheme (CES). `string` is encoded using the
CES before percent-encoding. This argument is passed to `babel:string-to-octets`
without any change.

`encoding`は文字符号化方式（CES）です。`string`はパーセントエンコーディングの前
にそのCESを使ってエンコードされます。この引数は`babel:string-to-octets`にそのま
ま渡されます。

#### Function: decode string &key test www-form encoding

Decodes `string` according to percent-encoding and returns it as a string.

`string`をパーセントエンコーディングに従ってデコードし、文字列として返します。

`test` is a function of one argument that returns a generalized boolean. It's
used for determining whether each octet requires decoding. If it returns true,
the octet is decoded. The default value is `(constantly t)`.

`test`はひとつの引数を持ち、真偽値を返す関数です。それぞれのオクテットにデコー
ディングが必要かどうかを決めるために使われます。真を返した場合、そのオクテットは
デコードされます。標準の値は`(constantly t)`です。

If `www-form` is true, assumes that `string` is an
application/x-www-form-urlencoded string.

`www-form`が真の場合、`string`をapplication/x-www-form-urlencodedでエンコードさ
れた文字列とみなします。

`encoding` is a character encoding scheme (CES). `string` is decoded according
to the CES after percent-decoding. This argument is passed to
`babel:octets-to-string` without any change.

`encoding`は文字符号化方式（CES）です。`string`はパーセントエンコーディングをデ
コードした後に、そのCESに従ってデコードされます。この引数は
`babel:octets-to-string`にそのまま渡されます。

### Predicates

#### Function: gen-delims-p x
#### Function: sub-delims-p x
#### Function: reservedp x
#### Function: alphap x
#### Function: digitp x
#### Function: unreservedp x
#### Function: userinfop x
#### Function: reg-name-p x
#### Function: pcharp x
#### Function: queryp x
#### Function: fragmentp x

Returns true if the octet `x` is a member of each character set. See RFC 3986.

オクテット`x`がそれぞれの文字集合に属する場合に真を返します。RFC 3986を見てくだ
さい。

Acknowledgements
----------------

The API of percent-encoding was inspired by Daniel Oliveira's
[do-urlencode](https://github.com/drdo/do-urlencode) and Franz's
[uri](https://github.com/franzinc/uri).

percent-encodingのAPIはDaniel Oliveiraさんの
[do-urlencode](https://github.com/drdo/do-urlencode)とFranzの
[uri](https://github.com/franzinc/uri)を参考にしています。

sile's [url](http://d.hatena.ne.jp/sile/20091216/1260980935) gave some important
hints for speed to me.

sileさんの[url](http://d.hatena.ne.jp/sile/20091216/1260980935)から速度面でのヒ
ントをもらいました。
