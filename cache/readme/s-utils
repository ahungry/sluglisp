# S-UTILS

# A collection of Common Lisp utilities

`S-UTILS` is collection of Common Lisp utilities. This simple package is used as a building block in a number of other open source projects.

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

`S-UTILS` helps in:

-   manipulating directory pathnames
-   copying streams
-   doing some elementary parsing (tokenising)
-   flexibly formatting dates, times and durations
-   parsing integers more safely

### Status

`S-UTILS` is considered stable code.

### News

*November 2005* - Created as a separate project.

### Platforms

`S-UTILS` is written in ANSI standard Common Lisp and should be portable
across any CL implementation.

### Installation

The `S-UTILS` package is loaded using [ASDF](http://www.cliki.net/asdf).
There is an excellent [tutorial on
ASDF](http://constantly.at/lisp/asdf/) to get you started.

    CL-USER 1 > (asdf:oos 'asdf:load-op :s-utils)

Example of setting up and using ASDF to compile and load the package

### Usage

Usage of the functionality in `S-UTILS` is straightforward, as the
following examples demonstrate:

    CL-USER 1 > (in-package :s-utils)
    #<The S-UTILS package, 41/128 internal, 13/16 external>

    S-UTILS 2 > (defvar *timestamp* (get-universal-time))
    *TIMESTAMP*

    S-UTILS 3 > (format-universal-time *timestamp*)
    "Sat Nov 19 2005 18:08:51"

    S-UTILS 4 > (format-iso-gmt-time *timestamp*)
    "20051119T170851"

    S-UTILS 5 > (format-universal-time *timestamp* :format +us-date-format+ :stream t) 
    Sat Nov 19 2005
    NIL

    S-UTILS 6 > (format-universal-time *timestamp* :format +us-time-format+ :timezone 0)
    "Sat Nov 19 2005 17:08:51"

    S-UTILS 7 > (format-duration (- (get-universal-time) *timestamp*))
    "5 minutes 18 seconds"

    S-UTILS 8 > (tokens *)
    ("5" "minutes" "18" "seconds")

    S-UTILS 9 > (tokens "1-2-3-4-5" :separators '(#\-))
    ("1" "2" "3" "4" "5")

    S-UTILS 10 > (parse-integer-safely "")
    NIL

    S-UTILS 11 > (parse-integer-safely "x" :default -1)
    -1

    S-UTILS 12 > (parse-integer-safely "111" :default 123 :start 2)
    1

    S-UTILS 13 > (parse-integer-safely "111" :default 123 :start 3)
    123

    S-UTILS 14 > (parse-integer-safely "" :default -1)
    -1

    S-UTILS 15 > (parse-integer-safely nil :default -1)
    -1

    S-UTILS 16 > (parse-integer-safely "FF" :radix 16)
    255

    S-UTILS 17 > (with-input-from-string (in "abcdefghi")
                   (copy-stream in *standard-output*)
                   (terpri))
    abcdefghi
    NIL

    S-UTILS 18 > (make-subdirectory #p"/tmp/" '("foo" "bar"))
    #P"/tmp/foo/bar/"

    S-UTILS 19 > (pathname-parent #p"/tmp/")
    #P"/"

    S-UTILS 20 > (pathname-parent (make-subdirectory #p"/tmp/" "foo"))
    #P"/tmp/"

### API Reference

There is automatically generated documentation
available for the `S-UTILS` package in `doc/API.html`.

### Mailinglist

There is no mailing list for this project.

### Changelog

Release Notes:

-   release 1: moved `S-UTILS` into a seperate project under a new
    structure

### TODO

There is currently no TODO list.

### FAQ

Nothing appropriate.

### Bugs

There are no known bugs.

### Authors

`S-UTILS` was written by Sven Van Caekenberghe.

### Maintainers

`S-UTILS` is being maintained by Sven Van Caekenberghe.

### License

You are granted the rights to distribute and use this software as
governed by the terms of the Lisp Lesser General Public License
([http://opensource.franz.com/preamble.html](http://opensource.franz.com/preamble.html)),
also known as the LLGPL.

### History

This is a new project.

### References

Thera are no references.

Copyright © 2004-2005 Sven Van Caekenberghe, Beta Nine BVBA. All Right
Reserved.