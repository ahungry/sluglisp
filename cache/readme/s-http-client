# S-HTTP-CLIENT

## A Common Lisp HTTP Client

`S-HTTP-CLIENT` is a
basic implementation of a Common Lisp HTTP Client. This simple package
is used as a building block in a number of other open source projects.

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

`S-HTTP-CLIENT` can:

-   access any URL over HTTP/HTTPS using any HTTP verb
-   support connection reuse
-   support basic authentication

### Status

`S-HTTP-CLIENT` is considered stable code.

### News

-   *December 2005* - `S-HTTP-CLIENT` was featured in my first [Lisp
    Movie: Episode 1: HTTP Client and
    Server](http://homepage.mac.com/svc/LispMovies/index.html) [dead-link].
-   *November 2005* - Created as a separate project.

### Platforms

`S-HTTP-CLIENT` is written in ANSI standard Common Lisp and should be
portable across any CL implementation, provided parts of `S-SYSDEPS` are
ported.

### Installation

The `S-HTTP-CLIENT` package is loaded using
[ASDF](http://www.cliki.net/asdf). There is an excellent [tutorial on
ASDF](http://constantly.at/lisp/asdf/) to get you started.

    CL-USER 1 > (asdf:oos 'asdf:load-op :s-http-client)

Example of setting up and using ASDF to compile and load the package

### Usage

Just call `DO-HTTP-REQUEST` with the necessary arguments. For example:

    CL-USER 1 > (in-package :s-http-client)
    #<The S-HTTP-CLIENT package, 59/128 internal, 5/16 external>

    S-HTTP-CLIENT 2 > (s-http-client:do-http-request "http://homepage.mac.com/svc/s-http-client/foo.html")
    "<html>
    <head><title>Foo</title></head>
    <body>
    <h1>Foo</h1>
    <p>Just a test page.</p>
    </body>
    </html>
    "
    200
    ((:DATE . "Sat, 19 Nov 2005 22:05:38 GMT") 
     (:CONTENT-LENGTH . "100") 
     (:CONTENT-TYPE . "text/html") 
     (:CACHE-CONTROL . "public") 
     (:X-RESPONDING-SERVER . "webdav11") 
     (:SERVER . "AppleDotMacServer") 
     (:ETAG . "14djc8on-8nlp-j19yhvvj5-c37cvzcpi0") 
     (:LAST-MODIFIED . "Sat, 19 Nov 2005 22:02:31 GMT") 
     (:VIA . "1.1 netcache02 (NetCache NetApp/5.5R6)"))
    #<URI http://homepage.mac.com:80/svc/s-http-client/foo.html>
    :NEW

In principle, any HTTP verb is allowed. It is the easiest to specify
them as keywords. In the case of :POST or :PUT, content as well as a
content-type should be specified (content length will be calculated).
Content can be specified as a string or byte sequence as far as
write-sequence can be called on your platforms TCP/IP stream. Basic
authorization is to be specified as (username . password) strings.

Connections are kept alive and reused as long as the same host and port
are accessed using the same scheme. Both HTTP/1.0 with Keep-Alive
headers as well as HTTP/1.1 is supported. Chunked transfer encoding is
also implemented.

### API Reference

There is automatically generated in documentation
available for the `S-HTTP-CLIENT` package in `doc/API.html`

### Mailinglist

There is no mailing list for this project.

### Changelog

Release Notes:

-   release 1: moved `S-HTTP-CLIENT` into a seperate project under a new
    structure

### TODO

-   Proxy support is currently not implemented, although it is not
    really hard.

### FAQ

Nothing appropriate.

### Bugs

-   HTTPS in only implemented on LispWorks (where it was really easy).

### Authors

`S-HTTP-CLIENT` was written by Sven Van
Caekenberghe.

### Maintainers

`S-HTTP-CLIENT` is being maintained by Sven Van
Caekenberghe.

### License

You are granted the rights to distribute and use this software as
governed by the terms of the Lisp Lesser General Public License
([http://opensource.franz.com/preamble.html](http://opensource.franz.com/preamble.html)),
also known as the LLGPL.

### History

This is a new project.

### References

There are no references.

Copyright © 2005 Sven Van Caekenberghe, Beta Nine BVBA. All Right
Reserved.