# S-HTTP-SERVER

## A Minimal Standalone Common Lisp HTTP Server

`S-HTTP-SERVER` is a minimal standalone HTTP Server. 
This simple package is used as a building block in a number of other open source projects.

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

`S-HTTP-SERVER` can:

-   handle HTTP requests and generate HTTP responses
-   be configured with plugins or handlers
-   has a builtin status handler
-   comes with a static resource handler, favicon handler and redirect
    handler
-   allows you to write and install your own handlers
-   support HTTPS on LispWorks

### Status

`S-HTTP-SERVER` is considered stable code.

### News

-   *April 2006* - added HTTPS support, serveral handlers and
    optimalizations.
-   *December 2005* - `S-HTTP-SERVER` was featured in my first [Lisp
    Movie: Episode 1: HTTP Client and
    Server](http://homepage.mac.com/svc/LispMovies/index.html) [dead link].
    Jean-François Brouillet has written an excellent [tutorial on using
    S-HTTP-SERVER](http://lisp.jfb-city.co.uk/tutorials/svc/s-http-server.html),
    complete with screenshots and code examples.
-   *November 2005* - Created as a new project.

### Platforms

`S-HTTP-SERVER` is written in ANSI standard Common Lisp and should be
portable across any CL implementation.

### Installation

The `S-HTTP-SERVER` package is loaded using
[ASDF](http://www.cliki.net/asdf). There is an excellent [tutorial on
ASDF](http://constantly.at/lisp/asdf/) to get you started.

    CL-USER 1 > (asdf:oos 'asdf:load-op :`S-HTTP-SERVER`)

### Usage

Basically, you create an `S-HTTP-SERVER` object and start it. Out of the
box only the status/debug page
[http://localhost:1701/s-http-server](http://localhost:1701/s-http-server)
is served. By registering new context handlers you can configure the
server further. The static-resource-handler hosts static documents from
a root directory in the file system. It is possible to write your own
handlers, look at the code of static-resource-handler and
`S-HTTP-SERVER`-handler for guidance.

    CL-USER 1 > (in-package :`S-HTTP-SERVER`)
    #<The `S-HTTP-SERVER` package, 90/128 internal, 31/64 external>

    `S-HTTP-SERVER` 2 > (defvar *server* (make-`S-HTTP-SERVER`))
    *SERVER*

    `S-HTTP-SERVER` 3 > (start-server *server*)
    ;; `S-HTTP-SERVER`: Started a new server on port 1701
    #<`S-HTTP-SERVER` "`S-HTTP-SERVER`" port 1701 running 10C5F6EB>

    `S-HTTP-SERVER` 4 > (register-context-handler *server* "/my-site" 'static-resource-handler :arguments '("/var/www/"))
    ((STATIC-RESOURCE-HANDLER "/my-site" "/var/www/") (`S-HTTP-SERVER`-HANDLER "/`S-HTTP-SERVER`" :BUILTIN))

    `S-HTTP-SERVER` 5 > (stop-server *server*)
    ;; `S-HTTP-SERVER`: Stopped server
    #<`S-HTTP-SERVER` "`S-HTTP-SERVER`" port 1701 not running 10C5F6EB>

Jean-François Brouillet has written an excellent [tutorial on using
S-HTTP-SERVER](http://lisp.jfb-city.co.uk/tutorials/svc/s-http-server.html), complete with screenshots and code examples.

### API Reference

There is automatically generated [API Reference](API.html) documentation
available for the `S-HTTP-SERVER` package.

### Mailinglist

The [KPAX mailing
list](http://common-lisp.net/cgi-bin/mailman/listinfo/kpax-devel) is
used for this project.

### Changelog

Release Notes:

-   release 1: moved `S-HTTP-SERVER` into a seperate project under a new
    structure

### TODO

There is currently no TODO list.

### FAQ

Nothing appropriate.

### Bugs

There are no known bugs.

### Authors

`S-HTTP-SERVER` was written by Sven Van
Caekenberghe.

### Maintainers

`S-HTTP-SERVER` is being maintained by Sven Van
Caekenberghe.

### License

You are granted the rights to distribute and use this software as
governed by the terms of the Lisp Lesser General Public License
([http://opensource.franz.com/preamble.html](http://opensource.franz.com/preamble.html)),
also known as the LLGPL.

### History

This is a new project.

### References

The reference for the HTTP protocol is [RFC
2616](http://www.w3.org/Protocols/rfc2616/rfc2616.html). Also worth reading
is [the Wikipedia article about
HTTP](http://en.wikipedia.org/wiki/Http).

Copyright © 2005, 2006 Sven Van Caekenberghe, Beta Nine BVBA. All Right
Reserved.
