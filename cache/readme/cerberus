# cerberus
A Common Lisp Kerberos (version 5) implementation.

This is an implementation of the Kerberos v5 authentication protocol in Common Lisp. The intention is to provide 
a robust, reliable and portable (across both Lisp implementations and host OSs) Kerberos authentication system. 
It has been developed/tested against the Windows KDC (i.e. active directory) running on SBCL under both Windows and Linux.

## 1. Introduction
Kerberos is the de facto standard method of authentication over a network, notably in Microsoft Windows environments.

The basic principal of Kerberos is there is a trusted central authority which stores credentials (password equivalents)
for each principal (user account). This is known as the Key Distribution Centre (KDC). 
A client can prove its identity to an application server by requesting a message from the KDC 
which is encrypted with the server's private key. Only the server (and the KDC) have the knowledge to decrypt this message,
the client itself does not. The client forwards this message to the server, who decrypts it and examines 
the contents of the message. Inside it will be some proof (e.g. a recent timestamp) that the client is who they say they are. 

In its simplest form, the Kerberos protocol consists of the following sequence of exchanges:
* Client sends a message to authentication server (AS) component of the KDC requesting a ticket for the ticket-granting server (TGS).
* The AS responds with a message encrypted with the client's private key, only the client can decrypt this message.
* The client sends a request to the TGS for a ticket for the desired principal (application server).
* The client sends this ticket to the application server using the relevant application protocol.
* The application server validates the ticket and approves access to the client.

The details get more complicated, but that is the general idea.

## 2. Project aims
- [x] Be able to encode/decode all the relevant DER-encoded ASN.1 messages 
- [x] Support a suffiently wide range of encryption profiles to be useful. In practise this means the ones supported by
Microsoft. 
- [x] Send AS-REQ messages to the KDC to get TGTs 
- [x] Send TGS-REQ messages to the KDC to get credentials for application servers
- [x] Encode/decode AP-REQ messages to send to application servers
- [x] Validate AP-REQ messages to authenticate clients
- [x] Wrap AP-REQ messages with the KRB5 OID, as required for GSS
- [x] Some sort of GSSAPI integration (sort of there, some polishing required)
- [x] Write a KDC server

## 3. Usage

Users should first "logon" by providing credentials and IP address of the KDC (Domain controller):
```
(logon-user "username@realm" "password" :kdc-address "10.1.1.1")
```
This modifies the global `*CURRENT-USER*` variable. Alternatively you may rebind this variable 
if you require a local change of user.
```
(with-current-user ((logon-user "username@realm" "Pasword" :kdc-address "10.1.1.1"))
  body)
```

Services, which do not require initial authentication with the KDC, should use 
```
(logon-service "service/host.name.com@realm" keylist)
```
where `KEYLIST` is a list of keys as returned from either `GENERATE-KEYLIST` or `LOAD-KEYTAB`.

### 3.1 GSSAPI
Kerberos authentication is then performed using the GSSAPI as provided by the [glass](https://github.com/fjames86/glass) 
package. 


```
;; ---------- client --------
CL-USER> (logon-user "username@realm" "password" :kdc-address "10.1.1.1")
;; acquire a client credential structure for the current user
CL-USER> (defparameter *client-creds* (gss:acquire-credentials :kerberos "service/host.name.com@realm"))
*CLIENT-CREDS*
;; initialize a context and generate a token buffer to send to the server
CL-USER> (multiple-value-bind (context buffer) (gss:initialize-security-context *client-creds* :mutual t)
	   (defvar *client-context* context)
	   (defvar *buffer* buffer))
*BUFFER*

;; -------- on the server -----
CL-USER> (logon-service "service/host.name.com@realm" *keylist*)
;; acquire a crednetial structure for the current user
CL-USER> (defparameter *server-creds* (gss:acquire-credentials :kerberos nil))
*SERVER-CREDS*
;; accept the context and generate a response token (if required)
CL-USER> (multiple-value-bind (context buffer) (gss:accept-security-context *server-creds* *buffer*)
	   (defvar *server-context* context)
	   (defvar *response-buffer* buffer))
*RESPONSE-BUFFER*

;; -------- client -----------
;; pass the token back to the client so it can validate the server
CL-USER> (gss:initialize-security-context *client-context* :buffer *response-buffer*)
```

### 3.2 KDC discovery
To discover the location of your KDC on the network, you should issue a DNS SRV query, e.g. using [dragons](https://github.com/fjames86/dragons):
```
CL-USER> (dragons:query (dragons:question "_kerberos._tcp.my.domain.com" :srv))
(#S(DRAGONS::RR
    :NAME "_kerberos._tcp.my.domain.com"
    :TYPE :SRV
    :CLASS :IN
    :TTL 600
    :RDATA (:PRIORITY 0 :WEIGHT 100 :PORT 88 :TARGET
            "myDC.my.domain.com")))
NIL
(#S(DRAGONS::RR
    :NAME "myDC.my.domain.com"
    :TYPE :A
    :CLASS :IN
    :TTL 1200
    :RDATA #(10 1 1 47)))
((:NAME "_kerberos._tcp.my.domain.com" :TYPE :SRV :CLASS :IN))
```

## 4. Encryption profiles
Cerberus supports a set of encryption "profiles", which are implemented by specializing a set of generic functions.

- [x] The simple DES-based profiles are all implemented and appear to be working, DES-CBC-MD5, DES-CBC-MD4 and DES-CBC-CRC.
- [x] The Microsoft profile RC4-HMAC is working correctly. RC4-HMAC-EXP has an unknown problem and is not working correctly.
It has temporarily been disabled.
- [x] The triple-des profile, DES3-CBC-SHA1-KD, is implemented and looks like it's working. 
- [x] The AES128 and AES256 profiles are working correctly.

## 5. Keytab files
You can load keytab files (as output from other Kerberos implementations, such from ktpass utility) using 
```
CL-USER> (cerberus:load-keytab "my.keytab")
```
This returns a list of KEYTAB-ENTRY structures, which include information about the principal as well as the 
encryption key. 

Note: there currently is no way to use the contents of a keytab file.

## 6. KDC
Cerberus now supports a simple KDC server. Each SPN (service principal name) is stored as an entry in a pounds 
database. The krbtgt principal is mandatory because this is the principal under which the TGS runs. So you must
first run
```
CL-USER> (cerberus-kdc:add-spn "krbtgt/MYREALM@MYREALM" "password")
```
before it can be used. Of course, you will also want to add other principals for each user and service. 

### 6.1 Example 

Add some SPNs and start the server:
```
CL-USER> (cerberus-kdc:add-spn "krbtgt/FRANK@FRANK" "mykdcpassword")
CL-USER> (cerberus-kdc:add-spn "frank@FRANK" "1234")
CL-USER> (cerberus-kdc:add-spn "dave@FRANK" "4321")
CL-USER> (cerberus-kdc:start-kdc-server "FRANK")
```

On the client, logon and get a ticket:
```
CL-USER> (cerberus:logon-user "frank@FRANK" "1234" :kdc-address "10.1.1.1")
CL-USER> (gss:acquire-credentials :kerberos "dave@FRANK")
```

### 6.2 RPC interface
The Cerberus KDC supports an RPC interface for configuration over the network. It is defined in kdc.x 
and implemented in kdc.lisp. Clients MUST be authenticated using AUTH-GSS:

```
CL-USER> (defparameter *client* (make-instance 'frpc:gss-client :credentials (gss:acquire-credentials :kerberos "krbtgt/FRANK@FRANK") :program 901980025 :version 1 :host "10.1.1.1"))
CL-USER> (cerberus-kdc:call-find "frank@FRANK" :client *client*)
```

## 7. TODO
- [ ] Need to be able to renew tickets (written the function but does it work?)
- [x] Somehow need to be able to use this in an application that requires GSS support.
- [x] Need to support encrypting application messages using the (sub)session key.
- [ ] Some sort of credential cache, i.e. database of TGTs and tickets for other principals.
- [ ] Support cross-realm requests and tickets.
- [ ] Need to support sub-session keys. At the moment it is assumed only the session key is available.
- [ ] A persistent credential cache? Could use the serializer to write the tickets out to a file.
- [ ] Support forwarding/forwarded TGTs. This is required to fully enable Single-Sign-On semantics, 
since it allows the application server to request tickets as if it were the client, effecitively forwarding the 
client's identity to the application server.
- [ ] It is impossible to know for sure what the correct salt to use is when requesting a TGT because it may not
be the default. We should therefore detect a KRB-ERROR-T (PREAUTH-FAILED) and inspect the data, this should
include an ETYPE-INFO or ETYPE-INFO2 structure which contains the real salt. Maybe we can just provide a suitable
restart?

## 8. Notes
* Both the DER serializer and the encryption functions cons A LOT.
* The ASN.1 serializer is specific to this project and NOT a generalized ASN.1 (DER) serializer. It makes certain assumptions which are valid
in the context of Kerberos messages, but are not generally applicable. Perhaps it could form the basis of one in the future.
* This was developed and tested against the Windows KDC (i.e. active directory). It should work with other KDCs such as MIT and Heimdal, 
but I've not tried. It is also internally consistent, so it works with the cerberus KDC as well.
* Need to understand the MS-PAC structures, these contain authorization data that is likely to be very useful. 
* Need to think more clearly about how to properly handle credential caching. What's there at the moment is broken, I have disabled it for the moment. Really it should be persisted anyway (write them out to a file?). 

## 9. License
Licensed under the terms of the MIT license.

Frank James 
April 2015.

