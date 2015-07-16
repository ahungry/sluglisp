# trivial-extract

[![Build Status](https://travis-ci.org/eudoxia0/trivial-extract.svg?branch=master)](https://travis-ci.org/eudoxia0/trivial-extract)

You have an archive. Maybe it's a `.tar`, or a `.tar.gz`, or `.zip`. You don't
know and don't care. You want its contents out.

# Usage

Contents are extracted to the file's containing directory. All functions return
`t` on success.

~~~lisp
;; Best effort, do what I mean
(trivial-extract:extract #p"~/path/to/file.{tar|tar.gz|zip}")

;; Content-specific
(trivial-extract:extract-tar #p"~/file.tar")
(trivial-extract:extract-gzip #p"~/file.tar.gz")
(trivial-extract:extract-zip #p"~/file.zip")
~~~

# License

Copyright (c) 2014-2015 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the MIT License.
