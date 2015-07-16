# lowlight

A simple syntax highlighter in Common Lisp based on a lexer and a parser.

*Version 1.0 approaching!*

## Features

* higlighting code in html spans
* themable with css
* string and file highlighting
* highlight code in markdown blocks
* simple and flexible highlighting engine
* based on a lexer ([graylex]) and a parser ([cl-yacc])

### Supported languages

* Common Lisp

I will probably support more languages in the future,
but you can also define your own language styles.
Also, patches to get your language style included are welcome.

## Installation

Lowlight is already available in Quicklisp, but at the time of writing this
you will find an old version there.
If you want the latest version lowlight, please download the source and
load it via ASDF (or as a local Quicklisp project), or make sure.
You know, that the Quicklisp version has been updated to the current version,
if it defines a package `lowlight.1`.

## Usage

All lowlight functions and macros live in the package `lowlight`:

```common-lisp
(in-package #:lowlight)
```

To highlight a string use `light`:

```common-lisp
(light :common-lisp ":bla") ;=> "<span class=\"keyword\">:bla</span>"
```

If you want to highlight a whole file, use `light-file`:
```common-lisp
(light-file :common-lisp "~/lowlight.lisp" :css "github-colors.css")
```

## Known Bugs an Limitations

### Lexing problems

As lowlight uses [graylex] for lexing,
it currently suffers from a bug related do the buffer size graylex uses.
If you experience Problems with the lexer not correctly recognizing tokens
that cross the 1024 or 2048 character border, try to increase the buffer size
`lowlight:*lexer-buffer-size*`.

### Parsing problems

The grammars that are created by `define-cfg-style` are ambiguous by design,
so conflict warnings will be ignored by default. If you run into problems, please
consider using `define-simple-style` or tell me, how to create unambiguous grammars
in `define-cfg-style`. Maybe I will provide a third macro to explicitly specify a
complete cfg, so you can make sure it's unambiguous yourself (if anyone actually needs this).

[graylex]: https://github.com/e-user/graylex
[cl-yacc]: http://www.pps.univ-paris-diderot.fr/~jch%20/software/cl-yacc/