# glass
General Lisp Authentication Security Services (glass) is a Common Lisp GSS-compatible API. It provides a set of 
generic functions which systems providing authentication services should specialize. Users wishing to consume
these services should use these rather than functions exported directly from the providing packages. 

The supported authentication systems are Kerberos, NTLM and SPNEGO (Negotiate). 

## 1. Introduction
The GSSAPI specifies a generalized mechanism for defining security service APIs. It is the most common way 
to consume Kerberos authentication. 

## 2. Usage
This package provides a set of generic functions. Systems which provide security systems should provide 
methods for these generics.

### 2.1 Kerberos
Kerberos support is provided by [cerberus](https://github.com/fjames86/cerberus).

```
;; client
CL-USER> (cerberus:logon-user "username@realm" "password" :kdc-address "10.1.1.1")
CL-USER> (defvar *credentials* 
                 (gss:acquire-credentials :kerberos 
                                         "host/host.name.com@realm"))
*CREDENTIALS*
CL-USER> (multiple-value-bind (context buffer) (gss:initialize-security-context *context* :mutual t)
           (defvar *client-context* context)
           (defvar *buffer* buffer))

;; send the buffer to the application server
CL-USER> (cerberus:logon-service "host/host.name.com@realm" "password")
CL-USER> (defvar *server-credentials* (gss:acquire-credentials :kerberos nil))
*SERVER-CREDENTIALS*
CL-USER> (multiple-value-bind (context response-buffer) (gss:accept-security-context *server-credentials* *buffer*)
            (defvar *server-context* context)
            (defvar *response-buffer* response-buffer))

;; send the response buffer back to the client and pass to INITIALIZE-SECURITY-CONTEXT so the 
;; client can authenticate the server
CL-USER> (gss:initialize-security-context *client-context* :buffer *response-buffer*)

;; compute checksums
CL-USER> (gss:get-mic *client-context* #(1 2 3 4))
CL-USER> (gss:verify-mic *server-context* (gss:get-mic *client-context* #(1 2 3 4)))

;; encrypt message
CL-USER> (gss:wrap *client-context* #(1 2 3 4))
CL-USER> (gss:unwrap *server-context* (gss:wrap *client-context* #(1 2 3 4)))

```

### 2.2 NTLM 
NTLM support is provided by [ntlm](https://github.com/fjames86/ntlm). NTLM is a legacy protocol
and is not recommended for use over unsecure networks, nevertheless it is often required for use with
various Microsoft tools.

### 2.3 SPNEGO (Negotiate)
Negotiate support is provided by [spnego](https://github.com/fjames86/spnego). This system is essentially a
wrapper around NTLM and Kerberos, with an initial negotiation stage to determine a mutually agreeable system.

## 3. License
Licensed under the terms of the MIT license.

Frank James 
May 2015.

