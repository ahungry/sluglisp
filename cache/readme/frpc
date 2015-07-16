# frpc
This is an implementation of the ONC-RPC ("SunRPC") protocol in Common Lisp. It provides both a generalized 
eXtensible Data Representation (XDR) serializer and a flexible Remote Procedure Call (RPC) framework to build robust, secure networked 
services. It supports the most commonly used authentication flavours (see below) including RPCSEC_GSS (i.e. Kerberos).

See the related project [nefarious](https://github.com/fjames86/nefarious), which uses frpc to implement an NFSv3 client and server.

An XDR protocol compiler is also included, which provides similar functionality to rpcgen typically used with the C programming language (see section 9).

## 1. Defining RPC interfaces
RPC interfaces are given a unique integer called a program number, see 
[IANA](http://www.iana.org/assignments/rpc-program-numbers/rpc-program-numbers.xhtml) 
for a detailed list. Each program may have multiple versions of its interface, with each version 
having a different set of functions/arguments. Each procedure in the interface is also given a 
unique integer. Together these 3 integers define the procedure identifier.

In frpc, both clients and servers must define the interface. This supplies argument and result types.
Servers must additionally implement handlers for each procedure they wish to support.

For instance, a client should write:
```
(defrpc call-hello 0 :string :string)
(defrpc call-goodbye 1 :uint32 :string)
```

This defines a Lisp function for each RPC proc to call out to an RPC server to execute the procedure, e.g.
```
CL-USER> (call-hello "Bob" :host "10.1.1.1" :port 1234 :protocol :udp)
"Hello, Bob!"
```

Servers should additionally implement handlers for the procedures they wish to support
```
(defun handle-hello (msg)
  (format nil "Hello, ~A!" msg))

(defrpc call-hello 0 :string :string
  (:handler #'handle-hello))

(defun handle-goodbye (u)
  (format nil "Goodbye ~A!" u))

(defrpc call-goodbye 1 :uint32 :string
  (:handler #'handle-goodbye))

```
The :HANDLER option specifies a function which accepts a single argument, which will be the value decoded according
to the rule defined for the arg-type by the DEFRPC form. The handler function should return a single value which will be 
passed to the result-type serializer defined by the DEFRPC form. If the handler signals an error, 
the RPC server will be silent (i.e. not send a reply). Typically, you should indicate to the caller that an error occured
by returning a status code as a part of your return value. Most RPC interfaces define return value structures in this way.

The types provided to DEFRPC can be a generalized type specifier, as described below in section 4.5.

## 2. Client

The DEFRPC macro defines a wrapper around the underlying CALL-RPC function, with default values provided 
for the argument writer, result reader, program, version and proc arguments. 

Thus, with the example above, the client will be able to call a remote RPC server using, e.g., 

```
(call-hello "hello" :host "10.1.1.1" :port 8000)
```

The default client function accepts a single mandatory argument, which must match the corresponding XDR type specifier.
However, typically the programmer would like to provide a better interface to hide the underlying implementation. 
For instance, consider a procedure which accepts and returns a structure of two strings:
```
(defxstruct my-arg ()
  (a :string)
  (b :string))
(defrpc call-my-proc 1 my-arg my-arg
  (:arg-transformer (a &key (b ""))
    (make-my-arg :a a :b b))
  (:transformer (res)
    (values (string-upcase (my-arg-a res)) (string-upcase (my-arg-b res))))
  (:documentation "Accepts two strings A and B. Returns (values (string-upcase A) (string-upcase B))."))
```
The resulting client function can then be called:
```
CL-USER> (call-my-proc "Alice" :b "Bob" :host "10.1.1.1" :port 1234 :protocol :udp)
"ALICE"
"BOB"
```

The :ARG-TRANSFORMER option specifies how to transform the arguments to the function into arguments for 
the RPC call. You may use this to allow some of the arguments be passed in as keyword parameters. 
The :TRANSFORMER option specifies how to transform the result of the RPC call back to Lisp.
Documentation strings can be provided with the :DOCUMENTATION option.

### 2.1 CALL-RPC

The low-level client functionality is provided by CALL-RPC. This function is used by a client to 
send an RPC request to a remote server and blocks until a reply is received.

### 2.2 TCP connections

You can provide a connection to the functions defined by DEFRPC. This makes it more efficient to send multiple 
messages to a single server, without having to reconnect for each request.

Use RPC-CONNECT and RPC-CLOSE to establish and close a connection. The macro WITH-RPC-CONNECTION can be used to ensure the connection is closed on exit.

```
;; normal way to do it. establishes a connection and closes it at the end 
(frpc.bind:call-dump "192.168.0.8" :protocol :tcp)

;; reuses a connection to the server 
(frpc:with-rpc-connection (c "192.168.0.8" 111 :tcp)  
  (list (frpc.bind:call-dump :connection c) 
        (frpc.bind:call-dump :connection c)))
```

### 2.3 UDP 

Specifying :UDP as the protocol will send the message using the UDP transport instead of TCP (UDP is the default). If you care about the result, then specify a timeout in seconds to wait for the reply. If no reply is received an RPC-TIMEOUT-ERROR will be signalled, otherwise the function returns immediately with result nil.

```
(frpc.bind:call-null :host "192.168.0.1" :protocol :udp)
```

Users may also supply a connection argument for UDP so that they don't need to keep making new UDP sockets for each RPC.
```
(with-rpc-connection (conn host port :udp)
  (frpc.bind:call-null :protocol :udp :connection conn)
  (frpc.bind:call-dump :protocol :udp :connection conn))
```

### 2.4 UDP broadcast

You may send messages using UDP broadcast to find services on your local network.

```
(frpc.bind:call-null :host "255.255.255.255" :protocol :broadcast)
```

Broadcast messages return a list of the responses received within the timeout -- no timeout error 
is raised if no replies are received. Each element in the list is a list of 2
items (host result), where host is where the response came from 
and result is the result of decoding the message that was received.

Note: not all implementations support UDP broadcast. Check with usocket to find out whether your implementation is supported. 

## 3. RPC Server

An RPC server runs from within a single thread and listens on a set of TCP and UDP ports. 
It may serve a subset of available RPC programs, by default serving all programs. 

```
;; make a server instance
(defvar *server* (make-rpc-server :tcp-ports '(8000) :udp-ports '(8000)))

;; start the server in a new thread, it will listen for requests on TCP and UDP ports 8000
(start-rpc-server *server*)

;; stop the server thread
(stop-rpc-server *server*)
```

When the server accepts a TCP connection, it is added to a list of currently open connections. 
The server will select a connection to process using USOCKET:WAIT-FOR-INPUT. This allows the server to
keep open multiple TCP connections without blocking other traffic. Note that the socket IO is 
still synchronous. Connections which are idle for TIMEOUT seconds (default 60 seconds) are closed by the RPC server. 

### 3.1 Server handlers

The handler function, which is invoked to process an RPC request, should return an object which matches the type specified in the associated DEFRPC 
form. If the handler signals an RPC-AUTH-ERROR, the request will be rejected with the auth-stat provided (or AUTH-TOOWEAK otherwise). 

It is the handler's responsibility to ensure both that the caller has authenticated to a suffiently secure level and that 
the caller is authorized to call the function. 

If any other error is signalled, then the RPC server will be silent, i.e. not return any response to the caller. 
Some APIs require this behaviour, this is the way server handlers should support it.

Please note that because the RPC server is singly threaded, your handler function must not block execution because that will prevent 
the server from processing other requests. If your handler needs to do work which takes an extended period of time 
(lots of disk IO, making other RPCs etc.) then you should design your API in such a way that the initial call returns 
immediately with the work taking place in another thread; the client can poll for progress or 
be notified on completion (e.g. via a callback RPC).

### 3.2 Restricting programs
By default, an RPC server will serve all available programs. However, it might be that you want
only a subset of defined programs to be served from a particular RPC server. For instance, you might want an NFS server 
to run from one thread and your portmap to run from another. To do this, supply a list of program identifiers (integers, symbols or strings)
naming the programs you wish to run in that server.

```
CL-USER> (defparameter *portmap* (frpc:make-rpc-server :udp-ports '(111) :tcp-ports '(111) :programs '(100000)))
CL-USER> (defparameter *nfs* (frpc:make-rpc-server :programs '("nfs" "nfs.mount" "nsm")))
CL-USER> (frpc:start-rpc-server *portmap*)
CL-USER> (frpc:start-rpc-server *nfs*)
```

### 3.3 Listening on wildcard ports
If you don't supply any ports to `MAKE-RPC-SERVER` then a wildcard port will be selected for TCP and UDP (port number 0), this
allows for a randomly allocated high-numberd port to be used. You may enforce such befhaviour yourself by supplying 0 as a port number
if you wish. 

## 4. XDR serializer

The XDR serializer is largely decoupled from the rpc implementation. This means it 
could be used for other purposes as a generalised binary serialization system.

### 4.1 Primitive types

The primitive types which come predefined are:
* :int32 :uint32 :int64 :uint64 :octet
* :string
* :boolean
* :real32 :real64
* :void

You may define new primitive types using:
```
(defxtype name ((:reader reader-name) (:writer writer-name))
  ((stream) body-reading-value-from-stream)
  ((stream obj) body-writing-obj-to-stream))
```
Only very rare circumstances should require doing this.

The optional parameters READER-NAME and WRITER-NAME are the function names
generated for the type's reader and writer. If not provided, %READ- and %WRITE- 
prepended with the type's name are used.

Use XTYPE-READER and XTYPE-WRITER to lookup the type's reader
and writer functions.

Use READ-XTYPE and WRITE-XTYPE to read/write an instance of 
the type to/from a stream.

Use PACK/UNPACK to store/extract instances from buffers rather than streams.

### 4.2 enums

Enumerated types are lists of symbol/integer pairs. Define enum types using
```
(defxenum enum-name
  (symbol integer)
  ...)

;; example 
(defxenum myenum 
  (:a 0)
  (:b 1)
  (:c 2))
```

Lookup a corresponding integer or symbol using
```
(enum enum-type val)
```
where val is either an integer or a symbol.

### 4.3 unions

Discriminated unions are an enum followed by a value, with the type of the value specified by the provided enum.
Define union types using:
```
(defxunion union-type (enum-type)
  (enum-symbol type-name)
  ...
  (otherwise type-name))

;; example
(defxunion myunion (myenum)
  (:a :int32)
  (:b :string)
  (otherwise :void))
```
This essentially expands to a `CASE` form, so you may put an `OTHERWISE` clause at the end, usually this will be a `:VOID` type.

Make instances of unions using `MAKE-XUNION`, get the tag and value using `XUNION-TAG` and `XUNION-VAL` e.g.
```
(let ((u (make-xunion :a 123)))
  (values (xunion-tag u) (xunion-val u)))
:A
123
```

### 4.4 structures

Define structures using:
```
(defxstruct struct-name ()
  (slot-name xtype-name &optional initial-value)
  ...)
```
This expands to a DEFSTRUCT form to define a structure.

For smaller structures it may be more convenient to use lists or plists:
```
(defxtype* my-list () (:list :uint32 :string))
(defxtype* my-plist () (:plist foo :uint32 bar :string))
```

### 4.5 Generalized types

```
(defxtype* name ()
  form)
```

Where the FORM is:
* a symbol, naming another xtype
* (:list &rest forms) ::= a list of objects of each type 
* (:alist &rest (tag form)) ::= an alist of objects keyed on the tag
* (:plist &rest key form) ::= a plist of objects keyed on the key
* (:struct struct-name &rest (slot-name form)) ::= a structure with each slot of type form
* (:union enum-name &rest (enum-keys form))  ::= a union discriminated on the enum-name
* (:array form length) ::= a fixed-size array
* (:varray form &optional length) ::= a variable sized array, expands to a list of objects
* (:varray* form &optional length) ::= a variable sized array, expands to a vector of objects

These rules can be applied recursively. 

You may define global readers/writers using DEFREADER and DEFWRITER. These macros generate DEFUN forms. 
The equivalent macros using FLET are WITH-READER and WITH-WRITER.

### 4.6 XDR examples

```
;; defstruct and define reader/writer for it 
(defxstruct foobar ((:reader read-foobar) (:writer write-foobar))
  (x :string)
  (y :uint32))

;; serialize the structure to a file 
(with-open-file (f "foobar.dat" :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
  (write-foobar f (make-foobar :x "hello" :y 123)))
;; deserialize the stucture from a file 
(with-open-file (f "foobar.dat" :direction :intput :element-type '(unsigned-byte 8))
  (read-foobar f))
```

## 5. Authentication

The authentication system that was used for the request is bound to `*RPC-REMOTE-AUTH*` special 
variable in the context of an rpc handler function. This allows handlers to implemente *authorization*, 
i.e. determining whether the client is permitted to perform the action requested. 

Supported flavours:
- [x] AUTH-NULL: i.e. no authentication
- [x] AUTH-UNIX and AUTH-SHORT: uid/gid and machine name. Not really authentication as such, 
but a simple tagging mechanism. Provided directly by frpc.
- [x] AUTH-DES: public-key exchange verified by encrypted timestamps. This requires both the client 
and server have access to the public keys for each other. frpc implements its own system using a shared database
for local access and an RPC interface for remote access.
- [x] AUTH-GSS: GSS (i.e. Kerberos) authentication, supports authentication, integrity validation and privacy.
Uses the package [cerberus](https://github.com/fjames86/cerberus) to implement Kerberos v5 authentication. 
Provided by FRPC-GSS system.


To use client authentication you must create an instance of a subclass of RPC-CLIENT (see below). This can
also be used as a "bag" of default values for the CALL-RPC keyword parameters.

```
;; allocate a client 
CL-USER> (defvar *client* (make-instance 'frpc:rpc-client :host "10.1.1.1" :port 123 :protocol :udp :timeout 1))

;; use it to perform an RPC
CL-USER> (frpc.bind:call-null :client *client*)

```

### 5.1 AUTH-NULL 
This is the default mechanism and requires no special treatment.


### 5.2 AUTH-UNIX 
```
(defvar *client* (make-instance 'frpc:unix-client :uid 1000 :gid 1001 :gids '(1002 1005)))

;; the first call uses AUTH-UNIX and if successful will recive a nickname
(frpc.bind:call-null :client *client*)

;; subsequent calls use AUTH-SHORT i.e. the nickname
(frpc.bind:call-null :client *client*)

```

### 5.3 AUTH-DES
This is provided by the FRPC-DES system and should be loaded using, e.g.
```
CL-USER> (ql:quickload "frpc-des")
```

#### 5.3.1 Overview 
AUTH-DES authentication is based on a Diffie-Hellman key exchange. Each party (client and server) have a secret 
key, from which public keys are derived. The public keys are exchanged beforehand by some unspecified mechanism.
Traditionally this was implemented using an RPC service (defined by key_prot.x, see programs/keyserv.lisp) but this is somewhat award to use.
Instead frpc implements its own shared database of public keys, which is exported using RPC so that remote machines
can access its entries. Local processes can simply read from the database, remote processes use the RPC interface.

The local API is 
* FIND-PUBLIC-KEY name ::= lookup the public key for this name
* ADD-PUBLIC-KEY name public ::= store the public key for this name
* REMOVE-PUBLIC-KEY name ::= delete the entry for this name
* LIST-PUBLIC-KEYS ::= list all entries in the local database.

The RPC API is:
* CALL-GET name ::= lookup the public key for this name
* CALL-SET name public ::= set the public key for this name
* CALL-UNSET name ::= delete the entry for this name
* CALL-LIST ::= enumerate all entries

CALL-SET and CALL-UNSET may only be called by the named user and must have been authenticated using AUTH-DES. Note that 
this means the database entry can only be created using the local API. However, they can be modified/deleted remotely.

#### 5.3.2 Usage
Note that the client must know the name of the principal the service is running under. That has to be 
agreed in advance.

On the server:
```
;; open the database and ensure the database has an entry with the service name and secret key
CL-USER> (frpc-des:des-init "service-user" 123123123)
```

On the client:
```
;; get the public key for the service user
CL-USER> (defvar *service-public* (frpc-des:call-get "service-user"))

;; create a client
CL-USER> (defvar *client* (make-instance 'frpc-des:des-client :name "user-name" :secret 111122223333 :public *service-public*))

;; call a function, e.g. change my database entry
CL-USER> (frpc-des:call-set "user-name" (frpc-des:des-public 666666666) :client *client*)
```

### 5.4 AUTH-GSS (Kerberos)
This is provided by the FRPC-GSS system, load using e.g.
```
CL-USER> (ql:quickload "frpc-gss")
```

RPCSEC_GSS provides both integrity (checksumming) and privacy (encryption) of the call arguments/results. Set 
the :SERVICE level to `:INTEGRITY` for checksumming and `:PRIVACY` for encryption and checksumming of the call
arguments/results. The default is `:NONE` which sends the args/results as normal.

```
;; you must first logon before you can request credentials for the application server
CL-USER> (cerberus:logon-user "myusername@myrealm" "mypassword" :kdc-address "10.1.2.3")
CL-USER> (defvar *cred* (glass:acquire-credentials :kerberos "service/hostname.com@myrealm"))
;; make the instance of the gss client
CL-USER> (defvar *client* (make-instance 'frpc:gss-client :credentials *cred* :service :privacy))
;; attempt to call the function, this will first negotiate the authentication before calling the proc
CL-USER> (frpc.bind:call-null :client *client*)

;; the server should initialize itself with a credentials handle
CL-USER> (cerberus:logon-service "service/hostname.com@myrealm" "password")
CL-USER> (frpc:gss-init)
```

### 5.5 Reauthentication
RPC servers are free to flush their tables of allocated nicknames/handles. When this happens you will 
receive an RPC-AUTH-ERROR (AUTH-REJECTED) error. You should set your client back to its initial state and retry,
this should reallocate a new nickname/context handle.

```
CL-USER> (reinitialize-instance *client*)
```

### 5.6 Authorization
When server handlers are executed, the special variable `*RPC-REMOTE-AUTH*` is bound to the authenticator 
that was used in the request. This allows the server to decide whether to honour the request or 
to signal an RPC-AUTH-ERROR instead. 

You may call `RPC-AUTH-PRINCIPAL` to get a string representing the authenticated caller. This can be used to aid authorization.

## 6. Portmap/rpcbind
Typically each RPC service listens on a randomly allocated high-numbered port. In order to find out 
the port number to contact the service on you must first query a service which listens on a well-known port,
this service is called portmap or rpcbind. 

```
CL-USER> (frpc.bind:call-dump :host "10.1.1.1")
(#S(MAPPING :PROGRAM 100000 :VERSION 2 :PORT 111 :PROTOCOL :TCP)
 #S(MAPPING :PROGRAM 100000 :VERSION 2 :PORT 111 :PROTOCOL :UDP))
```

## 7. Examples

Several example programs are included. For more serious usages, see Nefarious, an NFS implementation in Common Lisp.

### 7.1 Hello world (hello.lisp)
This program shows the basics, provides a function to upcase a string. An xfile is also included for testing with rpcgen.
On Linux, 
```
$ rpcgen -a hello.x 
$ Make -f Makefile.hello
```
You will probably need to modify the client, hello_client.c, to pass in correct arguments. 

## 8. Logging 

Debug logging is provided by [pounds](https://github.com/fjames86/pounds). By default this will 
create a 2MB log file in your home directory named "frpc.log". You should change the path by modifying:

```
(setf frpc:*frpc-log-path* (merge-pathnames (user-homedir-pathname) "foo.log"))
```
The log is created on the first call to FRPC-LOG, this is typically when you make your 
first RPC call or start your RPC server.

By default only log messages with `:INFO` or greater level are actually written to the log. You can 
increase the logging verbosity by pushing `:TRACE` to `FRPC:*FRPC-LOG-LEVELS*`. 

For debugging and development you may follow the log to see output as it arrives:
```
(pounds.log:start-following *frpc-log*)

(pounds.log:stop-following)
```

Users may also write to this log if they wish, you should simply use a different tag (note: [nefarious](https://github.com/fjames86/nefarious)
shares the frpc log).

```
(let ((tag (babel:string-to-octets "MYLG")))
  (defun my-log (lvl format-control &rest args)
    (unless *frpc-log*		  
      (frpc-log :info "Initializing log"))
    (pounds.log:write-message *frpc-log* 
    			      lvl 		      
			      (apply #'format nil format-control args)
			      :tag tag)))
```

See the pounds documentation for more information on the logging system.

## 9. XDR parser/generator
Typically RPC interfaces are described by an "x-file" which is used as input into the program rpcgen which generates code for the C programming language. 
The system frpcgen (file gen/gen.lisp) provides a function to parse xfiles and generate a Lisp file with contents suitable for use with frpc.

This makes it easy to freely interoperate between Lisp and C services using RPC, because they will both be derived from the same definition.

Usage:
```
(frpcgen:gen "test.x")
```

This generates a file called "test.lisp" which contains Lisp code suitable for use with frpc. Some hand modifications will be probably be required 
to make the generated code more usable, but it should at least provide a reasonable starting point. 

## 10. Notes

* At the moment, reading from TCP streams requires buffering the input to cope with reading multiple fragments. This is REALLY bad if
large payloads are sent. A fragmented-stream type could be defined to wrap the underlying socket stream so that we can avoid the buffering on reads.
You still need to buffer writes because you need to know how much you intend to write before you've written it.
* Could make it easier to add more transports, e.g. SSL/TLS stream, writing to shared memory etc. Probably not much call for this though.
* UDP multicast? 
* Currently no way to define transient programs, i.e. programs which get assigned a random program number at runtime. 
* Need to support batching in some sort of useful way. So that a client can setup a connection and use wait-for-input to process replies. 
Basically need a pair of functions to send requests and receive replies from the connection.
* The XDR serializer is probably not as efficient as it could be, but who really cares so long as it works.
* Developed using SBCL on Windows. Also tested with Clozure CL on Windows, LispWorks on Windows and SBCL on Linux.

## 11. License

Released under the terms of the MIT license.

Frank James 
Febuary 2015.

