# Symbol Munger

A Common Lisp library to make it easy to change the capitalization and spacing of
a "sentence".  This is used for converting between space & capitalization 
rules of various systems.

* Converts to and from lisp, english, underscores and camel-case
 * Useful for converting database column names to lisp-symbols and english column header names
 * Useful for converting between common lisp variables names and javascript variable names
 * Removes excess word separators along the way (:foo--bar => "Foo Bar")
 * All conversion "->" functions accept lisp-trees of strings, symbols, and other lisp objects.
  * all lisp objects (other than the tree structure which is flattened) are converted by 
    symbol-munger::%coerce-to-string into strings.  
  * Each object part is treated as a separate word

## API
### normalize-capitalization-and-spacing

(s &key (capitalize :each-word) (word-separators #\space) 
        word-separators-to-replace stream in-place)
* Will recapitalize a string and replace word-separators with a
  standard one (in-place if desired and possible)
* If s is a lisp tree, then each part will be %coerce-to-string'ed and
  treated as a separate part of the phrase being normalized (ie: each
  part implicitly starts a new word)
* Will write to a stream if given it otherwise.
* Defaults to capitalizing each word but can be any of
 * {:each-word :first-word T (:all is an alias for T) nil :but-first-word (likeJavaScript) }
* word-separators are used to distinguish new words for the purposes of capitalization
 * The first of these will be used to replace word-separators-to-replace
* word-separators-to-replace helps normalize word separators so that spaces or underscores become the appropriate word-separator.
 * If this contains :capitals it assumes capital letters indicate a new word separation

returns a string (new or the one passed in if in-place) unless :stream is provided"

### lisp->english

Converts a common lisp symbol (or symbol-name) into an english phrase

:a-symbol-like-this becomes "A Symbol Like This"

### english->lisp-symbol & english->keyword

Converts an english phrase into a common lisp symbol (the keyword 
variant just specifies the keyword package)

"A Symbol Like This" becomes :a-symbol-like-this


### Many other similar functions

[See The Tests For Working Examples](https://github.com/bobbysmith007/symbol-munger/blob/master/tests/symbol-munger.lisp)

* english->lisp-symbol
* english->lisp-name
* english->keyword
* english->camel-case
* english->studly-case
* english->underscores

* lisp->english
* lisp->camel-case
* lisp->underscores
* lisp->studly-caps

* camel-case->english
* camel-case->lisp-name
* camel-case->lisp-symbol
* camel-case->keyword
* camel-case->underscores

* underscores->english
* underscores->lisp-name
* underscores->lisp-symbol
* underscores->keyword
* underscores->camel-case
* underscores->studly-caps

## Authors
 * [Acceleration.net](http://www.acceleration.net/) [Donate](http://www.acceleration.net/programming/donate-to-acceleration-net/)
  * [Russ Tyndall](http://russ.unwashedmeme.com/blog)
  * [Nathan Bird](http://the.unwashedmeme.com/blog)
  * [Ryan Davis](http://ryepup.unwashedmeme.com/blog)