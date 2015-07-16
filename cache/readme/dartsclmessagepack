DartsCLMessagePack
==================

This library implements a simple encoder/decoder for the MsgPack 
binary format (see http://msgpack.org/) All opcodes of the specification
are supported.

Unlike cl-messagepack, this library does not use the Ext family of 
opcodes. Also, this library is on a slightly lower level compared to
cl-messagepack. The advantage is, that this library refuses to guess
the type of objects being encoded, and also allows the application to 
choose a suitable representation for the values read on a case-by-case
basis.

Application Interface
----------------------

Writing

 - write-packed-null STREAM => UNSPECIFIC
 - write-packed-boolean VALUE STREAM => UNSPECIFIC
 - write-packed-integer VALUE STREAM => UNSPECIFIC
 - write-packed-single-float VALUE STREAM => UNSPECIFIC
 - write-packed-double-float VALUE STREAM => UNSPECIFIC
 - write-packed-string VALUE STREAM &key START END ENCODING => UNSPECIFIC
 - write-packed-octet-array-header LENGTH STREAM => UNSPECIFIC
 - write-packed-octet-array VALUE STREAM &key START END => UNSPECIFIC
 - write-packed-array-header LENGTH STREAM => UNSPECIFIC
 - write-packed-map-header LENGTH STREAM => UNSPECIFIC
 - write-packed-extension-header TYPE LENGTH STREAM => UNSPECIFIC

Reading

 - read-packed-value-or-header STREAM => VALUE TYPE TAG
 - read-packed-octet-array-data LENGTH STREAM => ARRAY
 - read-packed-string-data LENGTH STREAM &key ENCODING => ARRAY
 - read-packed-value STREAM &key MAP-READER ARRAY-READER EXTENSION-READER ACCEPT-EOF DEFAULT ENCODING => OBJECT

