This library implements the base58 encoding algorithm. It's basically base64 but
with a smaller alphabet (58, as in the title) that doesn't include similar
looking characters,
[among other things](https://github.com/bitcoin/bitcoin/blob/master/src/base58.h).

The implementation is essentially a carbon copy of Gavin Andresen's
[Python code](https://bitcointalk.org/index.php?topic=1026.0).

# Usage

```lisp
cl-user> (base58:encode "this is a test")
"jo91waLQA1NNeBmZKUF"
cl-user> (base58:decode "jo91waLQA1NNeBmZKUF")
"this is a test"
```

# Tests

The encoded strings in the tests were created with Andresen's unmodified code,
so they are correct. Probably.

# License

Copyright (c) 2014 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the MIT License.
