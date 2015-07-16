hunchentoot-single-signon
=========================

This package implements SPNEGO implementation for Hunchentoot.

  - Author: Elias Martenson
  - Email: lokedhs@gmail.com

The main dependency is on the package cl-gss, which uses is an FFI
wrapper around the GSSAPI library.

Usage information
-----------------

First, make sure you have a keytab file that contains the service
credentials for the principal `HTTP/domain` where `domain` is the
fully qualified domain name of the HTTP server.

Before starting the Hunchentoot server, register the keytab with
GSSAPI using the following function:

```lisp
(cl-gss:krb5-register-acceptor-identity file)
```

Once this is done, every Hunchentoot handler function needs to be
wrapped in a call to `SPNEGO-AUTH`. Here is a simple example. Assuming
the original content of the handler looked like this:

```lisp
(defun handler-function ()
  (setf (hunchentoot:content-type*) "text/plain")
  "You should be authenticated here")
```

When enabling single sign-on, the content should look like this:

```lisp
(defun handler-function ()
  (hunchentoot-single-signon:spnego-auth #'(lambda (name)
                                             (setf (hunchentoot:content-type*) "text/plain")
                                             "You should be authenticated here")))
```

The callback is called with an instance of `CL-GSS:NAME` which can be
used for authorisation. You can retrieve the name of the princpial
using the function `CL-GSS:NAME-TO-STRING`. Check the cl-gss package
for further information.
