# spellcheck

Peter Norvig's spell corrector for Common Lisp.

The article
[How to Write a Spelling Corrector](http://norvig.com/spell-correct.html)
mentions Mikael Jansson and his implementation on
[GitHub](https://github.com/mikaelj/snippets/blob/master/lisp/spellcheck/spellcheck.lisp).

This project is based heavily on that code but packages it as an ASDF
installable library with the hope of getting it into the Quicklisp
repository.

Example

	CL-USER> (ql:quickload "spellcheck")
	...
	("spellcheck")
	CL-USER> (spellcheck:initialize)
    #<HASH-TABLE :TEST EQUAL :COUNT 29157 {100F1D7233}>
    CL-USER> (spellcheck:correct "supposidly")
    "supposedly"

Rob Blackwell    
July 2013


