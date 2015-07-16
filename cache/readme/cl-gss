cl-gss - Common Lisp bindings for GSSAPI
========================================

Author contact information
--------------------------

  - Elias Martenson
  - Email: lokedhs@gmail.com

Summary
-------

GSSAPI is an API which is designed to provide a standard API to
authentication services. The API itself is generic, and the system can
provide different underlying implementations. The most common one is
Kerberos, which has several implementations, the most common of which
is probably Active Directory.

Wikipedia has a more complete summary of GSS-API:
https://en.wikipedia.org/wiki/GSSAPI

Attempts has been made to make this API fit well into the Common Lisp
style, and also to work around some of the incompatibilities between
different implementations. However, there are several functionalities
that are not implemented, but these functions are generally not used
much. If you are missing something, please let me know, or better yet,
send a patch.

Examples
--------

On the initiating side, the function `INIT-SEC` must be called to
initialise the handshake. This function takes a single required
parameter, the service name of service to which you intend to connect.

```lisp
(cl-gss:init-sec "host@domain" :flags '(:mutual))
```

In this case, we're only passing a single flag, `:MUTUAL`. This flag
indicates that not only do I want to verify my identify with the
remote service. It should identify itself with me.

This call returns several values:

  - A boolean value indicating whether `INIT-SEC` expects a reply from
    the peer before the context is ready
  - The context that may or may not be ready for use (as indicated by
    the previous return value)
  - A byte array containing the data that should be passed to
    `ACCEPT-SEC` on the peer
  - A list of flags indicating what features are supported

Since we used the flag `:MUTUAL`, the first return value will be `T`,
since `INIT-SEC` needs to validate the identity of the peer.

The next step is to transfer the byte array that was returned as the
second return value to the peer and pass it to `ACCEPT-SEC`:

```lisp
(cl-gss:accept-sec buffer)
```

The call will return 5 values:

  - A flag indicating whether `ACCEPT-SEC` expects more data from the
    originating process. The behaviour of this flag is similar to that
    of the first return value from `INIT-SEC`.
  - The context that will be used to encrypt and decrypt messages
  - The name of the principal that initiated the handshake
  - A byte array that should be sent to the originating side and
    passed to `INIT-SEC`
  - A list of flags that describes what features are supported

The name that was returned as the third return value is in an opaque
form but can be converted to a string using the function
`NAME-TO-STRING`. This name can then be used for authorisation checks.

```lisp
(let ((user-name (cl-gss:name-to-string name)))
  (unless (equal user-name "some-name")
    (error "No permission to access service")))
```

Once `INIT-SEC` and `ACCEPT-SEC` have returned `NIL` as its first
return value, the context is ready to be used.

In order to encrypt a packet, use the function `WRAP`. It takes the
following arguments:

  - A context that had been previously created by `INIT-SEC` or
    `ACCEPT-SEC`.
  - A byte array that should be encrypted
  - A keyword argument `:CONF` specifying a generalised boolean that
    indicates whether the packet should be encrypted or not. If false,
    the packet will merely be authenticated, but the content itself
    will remain unencrypted. The default is `NIL`.

To decrypt a packet, use the function `UNWRAP`. This function takes
the following arguments:

  - A context that had been previously created by `INIT-SEC` or
    `ACCEPT-SEC`.
  - A byte array that should be decrypted
