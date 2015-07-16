# Session Token
###### A simple CPRNG-powered session token generator

This implementation cribs heavily from [Doug Hoyte's](https://github.com/hoytech) Perl-implemented [Session::Token](https://github.com/hoytech/Session-Token), and it uses his [Common Lisp ISAAC](http://hcsw.org/downloads/isaac.lisp) implementation (packaged [here](https://github.com/thephoeron/cl-isaac) and [here](https://github.com/Inaimathi/isaac); the first of these is available via `quicklisp`).

### Quick Start

    CL-USER> (ql:quickload :session-token)
    To load "session-token":
      Load 1 ASDF system:
        session-token
    ; Loading "session-token"
    
    (:SESSION-TOKEN)
    CL-USER> (defvar generator (session-token:make-generator))
    GENERATOR
    CL-USER> (funcall generator)
    "VFZhF1FcxjEXfNl3DQ9NbxtFwDzrRZNc"
    CL-USER> (loop repeat 10 collect (funcall generator))
    ("cwQKz6ADlDebXyEJCv7QdjZPPXOAPaqL" "hW0rRq08vWGfTtm0qAHcqfjewgqnqjF2"
     "PTeUDzzjMBE68zQQEyr1tYfJ06MbMtIW" "KCAbUIOqOaOFaxOqr2T8L6ACH9Te0Ycz"
     "C5todVcryAmpHzM5SZc0L4gboN3QKnEj" "jXKG9kzA4ECOV9FmLGGNh2fLXHxQzjtC"
     "gO4FzN3jXKZxMBX4BfcudTk5iAWwe3F5" "cIach4uqDiX0VhG5MI4NmEZ7JZmBOTyl"
     "K3LfxgOjQIK3Tk0zJJzCOUzjOpw4b8DS" "bDlXmHuICiiRYgLOORrBpfZgIqFMTfq8")
    CL-USER> 

### Usage

The default generator creates 32-character long base 62 (`a-z`,`A-Z`,`0-9`) tokens and seeds itself with `(isaac:init-kernel-seed)` (which won't work on Windows because it pulls from `/dev/urandom` or `/dev/arandom`).

##### `make-generator`

takes three keyword arguments that let you specify its alphabet, token length and initial seed. Returns a closure that returns the next generated token (and can optionally be re-initialized with a new seed).

- `:initial-seed` must be one of `(isaac:init-kernel-seed)` (the default, and recommended for production use), `(isaac:init-common-lisp-random-seed)` (which initializes using `cl:random`, and is thus insecure but at least available on Windows) or `(isaac:init-null-seed)` (intended only for testing purposes, because it will always generate the same token stream with equivalent alphabet and length settings).
- `:alphabet` must be a bag of characters either in the form of a vector of chars (as in `#(#\a #\b #\c)`) or a vanilla string (as in `"abc"`). The generator avoids mod bias on this alphabet regardless of size, but see the [Introducing Bias](https://github.com/hoytech/Session-Token#introducing-bias) notes from Doug's implementation. This library takes the same approach. In order make declaring specific alphabets easier, `session-token` also exports `char-range` (see next sub-section)
- `:token-length` must be an integer and defaults to 32. Each generated token will be this long.

Example invocations:

    (session-token:make-generator)
	(session-token:make-generator :token-length 64)
	(session-token:make-generator :alphabet "0123456789abcdef")
	(session-token:make-generator :initial-seed (cl-isaac:init-kernel-seed) :token-length 64 :alphabet "0123456789abcdef")

Example use:

    SESSION-TOKEN> (defvar generator (session-token:make-generator))
    GENERATOR
    SESSION-TOKEN> (funcall generator)
    "iyyiGgbALDzVCC7qX27oXR7pP89ZKOqy"
    SESSION-TOKEN> (funcall generator (cl-isaac:init-kernel-seed))
    "WdUBwtFG9sT49pEgkb3qTqiK2FzmYefm" ;; generated with the new seed
    SESSION-TOKEN>

##### `char-range`

takes a series of range terms and additional keyword arguments for specifc extra chars to be added or excluded.

- `:not` can be a list/vector/string of characters that will be filtered from the final result
- `:plus` can be a list/vector/string of characters that will be added to the final result

You can pass neither, either or both. If you pass *conflicting* `:not` and `:plus` arguments, `:plus` takes precedence, and the specified characters will be present in the result.

Example:

    (char-range (#\a :to #\z))
    (char-range (#\a :to #\z #\A :to #\Z #\0 :to #\9))
    (char-range (#\a :to #\z #\A :to #\Z #\1 :to #\9) :not "O0lI1")
    (char-range (#\a :to #\z #\1 :to #\9) :plus ",.;:!?")
