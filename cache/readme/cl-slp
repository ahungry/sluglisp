

CL-SLP
=========

Common Lisp CFFI bindings to the OpenSLP library. Used for discovering and advertising 
services over the Service Location Protocol (SLP). 

CL-SLP uses the package nickname SLP.


Usage
------

CL-SLP just wraps the OpenSLP functions, see http://www.openslp.org/doc/html/ProgrammersGuide/index.html
for the official API reference. 

Before loading the library, ensure your libslp.so (unix) or slp.dll (windows) library
is available to be loaded on your system. CL-SLP automatically pushes "C:/program files (x86)/OpenSLP/" onto
*foreign-library-directories* which should enable it to be loaded on Windows.

* Call SLP-OPEN before any other functions. This opens a handle to OpenSLP. It gets called automatically by the other functions
so you shouldn't need to call it directly. However, in multi-threaded environments you should call SLP-OPEN directly
before any other functions because the automatic check is not thread-safe.

* Call SLP-CLOSE when finished using the library, this closes the handle and frees the memory allocated both by OpenSLP and CL-SLP.

* Get SLP properties using GET-PROPERTY and GET-PROPERTIES. 
These return a specific property and all SLP properties respectively.

* Discover services using FIND-SERVICES or FIND-ALL-SERVICES
This returns a list of discovered service urls.

* Get service attributes using FIND-ATTRIBUTES
This returns a list of assoc lists for attributes of the given server

* Get available scopes using FIND-SCOPES

* Get discovered service types using FIND-SERVICE-TYPES
This returns a list of discovered server types. These can be used as input to SLP-FIND-SERVICES

* Discover all services of all types using FIND-ALL-SERVICES. 
This just maps over the server types and calls FIND-SERVICES

* Register a service using REGISTER 
The lifetime must be positive and less than *maximum-lifetime* = 65535. 
By default it is *default-lifetime* = 10800, which is the OpenSLP default.

* Deregister a service using DEREGISTER

* Attributes are represented as an assoc list and converted to a string using SLP-FORMAT-ATTRIBUTES. 
For API functions that accept the keyword parameter ATTIBUTES you can give either a string (formatted
using SLP-FORMAT-ATTRIBUTES) or an assoc list (alists are the recommended method).

Note that since SLP attributes map names to a list of values, this function accepts as input
an assoc list that maps names to either atoms or lists, e.g.
(slp-format-attributes '((:a . 123) (:b 321) (:c 123 456)) -> "(A=123),(B=321),(C=123,456)"

* CL-SLP generates Common Lisp errors of type SLP-ERROR. OpenSLP error codes are translated 
into Common Lisp SLP-ERROR objects.

Notes
------

The OpenSLP library makes extensive use of callbacks in its API. CL-SLP defines a default 
callback for each of the library calls, which it uses to collect return data. Users of CL-SLP
may if they wish define their own callbacks using the macros 
DEFINE-SERVER-TYPE-CALLBACK, DEFINE-SERVER-URL-CALLBACK, DEFINE-ATTR-CALLBACK and DEFINE-REGISTER-CALLBACK.

They can be called by passing the name of the new callback with the :callback-name keyword parameter to 
the functions that take it. 

The default callbacks should be sufficient for most needs because they just collect all the data
available and return it to Lisp.

Note also that you MUST use a Lisp that supports callbacks; CL-SLP was developed and tested using SBCL
on Ubuntu Linux and Windows 7.

All the callbacks take an optional argument, COOKIE, that is a pointer to an area of memory for use in
return values. This doesn't make much sense in Common Lisp, since we have other ways of returning data
from callbacks so it probably should be either ignored or removed from the Lisp calls.

* Error -19 NETWORK_TIMED_OUT
This error code seems to be returned on Windows 7 machines (possibly others) on REGISTER and
DEREGISTER, even though the call appears to be successul. Services registered are discsoverable
and can be deregistered again so appear to be working fine, even though these calls error.
CL-SLP therefore ignores this error, but prints a message to *error-output*.

API changes
------------

In earlier versions of CL-SLP the functions FIND-SERVICES, FIND-ALL-SERVICES and FIND-SERVICE-TYPES were
slightly misnamed as FIND-SERVERS, FIND-ALL-SERVERS and FIND-SERVER-TYPES. These functions remain but are
now deprecated.

Example
--------

```
;; Find service types
(slp:find-service-types)
-> ("service:wbem:https")

;; Find all servers
(slp:find-all-services)
-> ("service:wbem:https://localhost:5989")

;; Register a service 
(slp:register "service:myservice.x://localhost:8000" :attributes '((:num1 123) (:num2 321)))
-> T

;; Find services on the new service type
(slp:find-services "myservice.x")
-> ("service:myservice.x://localhost:8000")

;; Get attributes (try on a remote machine)
(slp:find-attributes "service:myservice.x://localhost:8000")
-> ((:NUM1 123) (:NUM2 321))

;; On a remote machine using slptool
;; $ slptool findattrs service:myservice.x://localhost:8000
;; (NUM1=123),(NUM2=321)

;; Deregister service
(slp:deregister "service:myservice.x://localhost:8000")
-> T

;; Check it has now gone
(slp:find-services "myservice.x")
-> NIL
```

Note that the OpenSLP daemon (slpd) must be running for register/deregister functions to work.


Frank James
March 2014

