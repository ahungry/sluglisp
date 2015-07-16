perlre
======

*perl regular expression api - m// and s/// - for CL-PPCRE with CL-INTERPOL support*
###### idea and code from Doug Hoyte's book [Let Over Lambda](http://letoverlambda.com) and quicklisp-package let-over-lambda

With **let-over-lambda** you can do:
- (#~m/regex/imsx string) or (#~s!regex!substitution!imsx string)
- It supports perl's imsx modifiers and arbitrary delimiters, 
- but for now it does not support "string-interpolation" in regex or substitution.


**perlre** supports: 
- perl's g modifier
- string- and backslash-interpolation with cl-interpol
- normal variable substitution or function calls like format
- suppressing interpolation, using single quotes `m''` or `s'''` as in perl
- ifmatch and whenmatch with ```$\` $& $\' $1, $2 ...``` note backslash in symbols with quotes


For now interpolation comes with a cost, here are some restrictions:
- regular expressions and substitutions must be explicit strings, i.e. double quoted or cl-interpol strings
- need of 2 backslashes, `\\& \\' \\1 \\2`... as cl-ppcre
- you must enable/disable cl-interpol-syntax

#### Examples:


```
(ql:quickload :perlre)


(#~s'(A)'*\1*'i "hanna")        ; => "h*a*nna"

(#~s'(A)'*\1*'ig "hanna")       ; => "h*a*nn*a*"

(#~s/"(a)"/"*\\1*"/ "hanna")    ; => "h*a*nna"

(#~s%"(a)"%"*\\1*"%g "hanna")   ; => "h*a*nn*a*"

(perlre:ifmatch (#~m/"(b)(c)(d)(e)"/ "abcdef") 
 (list $\` $& $\' $1 $2 $3 $4)) ; => ("a" "bcde" "f" "b" "c" "d" "e")

(perlre:whenmatch (#~m/"(b)(c)(d)(e)"/ "abcdef") 
  (print $\`) 
  (print $2) 
  (print $4))


------------
variable or function
------------

(let ((x "an")
      (y "AN"))
 (#~s/(format nil "~a" x)/(format nil "~a" y)/ "hanna")) ; => "hANna"

(let ((x "an")) 
 (pre:ifmatch (#~m/x/ "hanna") $&))                      ; => "an"

(let ((x "a(n)n")) 
 (pre:ifmatch (#~m/x/ "hanna") 
	(list $\` $& $\' $1)))                    ; => ("h" "ann" "a" "n")


------------
interpolation with cl-interpol
------------

(cl-interpol:enable-interpol-syntax)

(let ((x "(n)(n)"))
 (#~s/#?"${x}"/#?"\\1 \n\n \\2"/ "hanna"))

(let ((x "(n)(n)")
      (y "HANNA"))
 (#~s/#?"${x}"/#?"\\1 \n ${y} \n \\2"/ "hanna"))

(let ((x "an")) 
 (pre:ifmatch (#~m/#?"(${x})"/ "hanna") 
	(list $& $1)))

(cl-interpol:disable-interpol-syntax)

------------
with another quoting delimiter
------------

(#~s'(A)'*\1*'i "hanna")        ; => "h*a*nna"

(setf perlre::qd #\§)

(#~s§(A)§'\1'§i "hanna")        ; => "h'a'nna"


(setf perlre::qd #\') ; reset to default

```

#### There is a test-file with more examples:

```
(prove:run #P"path-to/perlre/test.lisp")
```
