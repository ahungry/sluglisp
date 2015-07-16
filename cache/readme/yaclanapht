anaphora
========

Yet another anaphoric macro package. Why?

Currently (as of 2013/02/24) other available anaphoric macro packages do not allow the
following sequence of steps to work.

        CL-USER> (ql:quickload 'yaclanapht)
        CL-USER> (yaclanapht:aif (+ 1 2) it 6)
        3

For things to work correctly, you should use the package, which basicaly means,
that you should create your own package, which prohibits script-like programming
style, found in Perl or Python.

This package is intended to fill the gap. With it, abovewritten code works
just as intended (i.e., works). It does so by interning IT during macroexpansion
in the caller's package, not definer's.

To make this not-using style even more convenient, nickname "A" is chosen for
this package. I believe, that in this case this is justifiable, since there
isn't really other clear candidate for the name except anaphoric macro collection package.

Also, some macros, found in classic books ("On Lisp", "Let over Lambda") work
as described there (for example, ALET), and not in some trivial noninteresting way.

TODO:
1) include analogs of all macros in ANAPHORA
2) include DEFANAPH from "On Lisp"
3) include ALET and ALET-FSM from "LOL"
4) include xyz-lambda reader macro.