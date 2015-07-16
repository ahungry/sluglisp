cl-secure-read
==============

Secure the lisp reader in spirit of Let Over Lambda. See section "Reader Security" on www.letoverlambda.com
to get the initial idea.

Example:

```lisp
CL-USER> (ql:quickload 'cl-secure-read)
CL-USER> (in-package cl-secure-read)
;; Define a function DEFAULT-RFS, which is a restricted version of READ-FROM-STRING
CL-SECURE-READ> (define-secure-read-from-string default-rfs :fail-value "caboom!")
CL-SECURE-READ> (default-rfs "123") ; this will read in number 123, as expected ...
;; ... and this will hopefully just return "caboom!",
;; not executing the removal shell-command.
CL-SECURE-READ> (default-rfs "#.(shell-eval \"rm -rf ./\"")
```

Now exports 4 macro:

  *  DEFINE-SECURE-READ-FROM-STRING - defines a function, which acts exactly like READ-FROM-STRING,
     only some macro-characters and dispatch-macro-characters (such as read-eval sequence #.) are disabled.
  *  DEFINE-SECURE-READ - same for READ or READ-PRESERVING-WHITESPACE
  *  SECURE-READ-FROM-STRING-LAMBDA - do not define READ-FROM-STRING-like function globally, but
     return a lambda instead
  *  SECURE-READ-LAMBDA - same for READ and READ-PRESERVING-WHITESPACE

Here are some notable parameters to macro, which control the behavior of resulting restricted reader:

  *  :READTABLE keyword, which allows you to specify, which readtable should your restricted reader-function use.
     Default is to take standard readtable.
  *  :BLACKLIST/:WHITELIST keywords, which specify, what macro-characters should be disabled/enabled.

     ```lisp
     ;; In this function read-eval is enabled, as well as comments
     (define-secure-read-from-string my-rfs :whitelist (#\; (#\# #\.) :allow-read-eval))
     ```
     Note, that black/white-list may contain sublists and keywords. Meaning of these will be explained below.

     Default black/white-list pair forces standard-io-syntax, disabled read-eval and
     disables all macro-characters except #\' #\, #\( and #\`
     (thus allowing only special syntax for construction of lists).

  *  SAFE-READ-FROM-STRING-WHITELIST and SAFE-READ-FROM-STRING-BLACKLIST variables can be used instead
     to specify whitelist and blacklist, by wrapping call to macro in LET.

     ```lisp
     ;; Same behavior, as in the previous example
     (let ((safe-read-from-string-whitelist '(#\; (#\# #\.) :allow-read-eval)))
       (define-secure-read-from-string my-rfs))
     ```

  *  :FAIL-VALUE is used to specify, what to return, when input contains disabled characters,
     default is to return NIL

Here is a full-fledged example, using most of the described features

```lisp
;; use readtable :clesh, allow comments, special clesh bang-syntax, allow read-eval,
;; do not force standard-io-syntax, in case of failure return string "caboom!"
(let ((safe-read-from-string-whitelist '(#\; #\! (#\# #\.) :allow-read-eval :keep-io-syntax)))
  (define-secure-read-from-string not-so-strict-read-from-string :readtable :clesh :fail-value "caboom!"))

(not-so-strict-read-from-string "asdf") ; this will read-in symbol ASDF
(not-so-strict-read-from-string "#(1 2 3)") ; and this will return "caboom!"
;; since we've requested not to force io-syntax, we may control read-eval dynamically.
;; Here returns "caboom!", even though *READ-EVAL* was enabled in the definition
(let (*read-eval*)
  (not-so-strict-read-from-string "#.(1 2 3)"))
```

Syntax of black/white-lists
---------------------------

Black/white list may contain:

  * characters, which are interpreted as macro-characters to deny/allow
  * lists of characters, which are interpreted as (macro-char ,@sub-macro-chars), where
    sub-macro-chars is a list of dispatch-macro-chars to deny/allow, which correspond to the given macro-char
    (usually #\#)
  * special keywords, which for now are the following
    :allow-read-eval - do not bind *READ-EVAL* to NIL explicitly
    :keep-io-syntax - do not wrap a call to READ-FROM-STRING into WITH-STANDARD-IO-SYNTAX
    :lists - allow/deny #\) and #\(
    :quotes - allow/deny #\` #\' and #\,
    All other keywords are ignored.
   
If BLACKLIST is NIL, all the macro-characters and dispatching macro-characters of the readtable
are disabled, unless they are explicitly enabled in the WHITELIST.
To actually enable all the macrocharacters in the readtable, use something like
```lisp
:BLACKLIST (:t)
```