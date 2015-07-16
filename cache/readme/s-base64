# S-BASE64

## A Common Lisp implementation of Base64 Encoding/Decoding

`S-BASE64` is an open source Common Lisp implementation of Base64 encoding and decoding. Base64 encoding is a technique to encode binary data in a portable, safe printable, 7-bit ASCII format. For a general introduction, please consult the [Wikipedia article on
Base64](http://en.wikipedia.org/wiki/Base64). 

This simple package is used as a building block in a number of other open source projects e.g. the [KPAX](https://github.com/svenvc/KPAX) web development framework.

### Contents

-   [Features](#features)
-   [Status](#status)
-   [News](#news)
-   [Platforms](#platforms)
-   [Downloading](#downloading)
-   [Installation](#installation)
-   [Usage](#usage)
-   [API Reference](#api)
-   [Mailinglist](#mailinglist)
-   [Changelog](#changelog)
-   [TODO](#tod)
-   [FAQ](#faq)
-   [Bugs](#bugs)
-   [Authors](#authors)
-   [Maintainers](#maintainers)
-   [License](#license)
-   [History](#history)
-   [References](#references)

### Features

`S-BASE64` can:

-   convert a Base64 encoded character input stream into a decoded
    binary output stream
-   convert a binary output stream into a Base64 encoded character
    output stream
-   convert a Base64 encoded character input stream into a byte array
-   convert a byte array into a Base64 encoded character output stream
-   optionally break lines at 76 characters

### Status

`S-BASE64` is considered stable code.

### News

*October 2005* - Created as a separate project.

### Platforms

`S-BASE64` is written in ANSI standard Common Lisp and should be portable
across any CL implementation.

### Installation

The `S-BASE64 package` is loaded using [ASDF](http://www.cliki.net/asdf).
There is an excellent [tutorial on
ASDF](http://constantly.at/lisp/asdf/) to get you started.

    $ cd apps/asdf/systems/
    $ ln -s ~/darcs/s-base64/s-base64.asd .
    $ cd ~
    $ /Applications/LispWorks/lispworks-tty 
    LispWorks(R): The Common Lisp Programming Environment
    Copyright (C) 1987-2005 LispWorks Ltd.  All rights reserved.
    Version 4.4.5
    Saved by sven as lispworks-tty, at 26 Oct 2005 11:53
    User sven on voyager.local
    ; Loading text file /Applications/LispWorks/Library/lib/4-4-0-0/config/siteinit.lisp
    ;  Loading text file /Applications/LispWorks/Library/lib/4-4-0-0/private-patches/load.lisp
    ; Loading text file /Users/sven/.lispworks
    ;  Loading text file /Users/sven/apps/asdf/init-asdf.lisp
    ;   Loading fasl file /Users/sven/apps/asdf/asdf.nfasl
    ;Pushed #P"/Users/sven/apps/asdf/systems/" onto ASDF central registry

    CL-USER 1 > (asdf:oos 'asdf:load-op :s-base64)
    ; Loading /Applications/LispWorks/Library/lib/4-4-0-0/load-on-demand/ccl/xp-fancyformat.nfasl on demand...
    ; loading system definition from
    ; /Users/sven/apps/asdf/systems/s-base64.asd into
    ; #<The ASDF787 package, 0/16 internal, 0/16 external>
    ; Loading text file /Users/sven/darcs/s-base64/s-base64.asd
    ; registering # as S-BASE64
    ;;; Compiling file /Users/sven/darcs/s-base64/src/package.lisp ...
    ...
    ; Loading fasl file /Users/sven/darcs/s-base64/src/package.nfasl
    ;;; Compiling file /Users/sven/darcs/s-base64/src/base64.lisp ...
    ...
    ; Loading fasl file /Users/sven/darcs/s-base64/src/base64.nfasl

Example of setting up and using ASDF to compile and load the package

### Usage

To encode you start with either a binary input stream or a byte array
and write to a character output stream. To decode you start from a
character input stream and write to a binary output stream or return a
byte array. You can use the standard CL marcros `WITH-OUTPUT-TO-STRING` of
`WITH-INPUT-FROM-STRING` to convert to and from a string. The following
listener transcript show how to compute the second example from RFC
3548, section 7:

    CL-USER 1 > (in-package :s-base64)
    #<The S-BASE64 package, 50/128 internal, 4/16 external>

    S-BASE64 2 > (setf bytes #(#x14 #xfb #x9c #x03 #xd9))
    #(20 251 156 3 217)

    S-BASE64 3 > (with-output-to-string (out) 
                   (encode-base64-bytes bytes out))
    "FPucA9k="

    S-BASE64 4 > (with-input-from-string (in *)
                   (decode-base64-bytes in))
    #(20 251 156 3 217)

Example Base64 Encoding and Decoding

### API Reference

There is automatically generated API Referencedocumentation
available for the `S-BASE64` package in `doc/API.html`.

### Mailinglist

The [KPAX mailing
list](http://common-lisp.net/cgi-bin/mailman/listinfo/kpax-devel) is
used for this project.

### Changelog

Release Notes:

-   release 1: moved `S-BASE64` into a seperate project under a new
    structure

### TODO

There is a variant of Base64 encoding used for URL's and filenames that
could be implemented.

### FAQ

Nothing appropriate.

### Bugs

Illegal input results in generic low-level CL conditions rather than a
more meaningful high-level application specific condition.

### Authors

`S-BASE64` was written by Sven Van
Caekenberghe.

### Maintainers

`S-BASE64` is being maintained by Sven Van
Caekenberghe.

### License

You are granted the rights to distribute and use this software as
governed by the terms of the Lisp Lesser General Public License
([http://opensource.franz.com/preamble.html](http://opensource.franz.com/preamble.html)),
also known as the LLGPL.

### History

`S-BASE64` was originally part of [KPAX](https://github.com/svenvc/KPAX) and became a separate project in
October 2005.

### References

The following RFC's can be considered as definitions of Base64 Encoding:

-   [RFC 1421](http://www.ietf.org/rfc/rfc1421.txt)
-   [RFC 2045](http://www.ietf.org/rfc/rfc2045.txt)
-   [RFC 3548](http://www.ietf.org/rfc/rfc3548.txt)

Copyright © 2002-2006 Sven Van Caekenberghe, Beta Nine BVBA. All Right
Reserved.