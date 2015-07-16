About Plump-Bundle
------------------
This is an implementation and specification of a binary file format to store [Plump](http://shinmera.github.io/plump/) documents.

How To
------
Storing happens through `output`, parsing through `input`.

    (plump-bundle:input (plump-bundle:output :vector (plump:parse "<test>Hey!</test>")))

They both accept namestrings, pathnames, vectors (`:vector` for ouput), or a [fast-io](https://github.com/rpav/fast-io) buffer.
The format handles all node-types defined in Plump-DOM 1.2 .

Format Specification (v1)
-------------------------
The following primitive types are declared:

    chunk     ::= type *
    children  ::= count chunk{count}
    map       ::= count pair{count}
    pair      ::= string string
    string    ::= count character{count}
    type      ::= integer
    count     ::= integer
    character --- A character encoded in utf-8.
    integer   --- Corresponds to (unsigned-byte 32), stored in big-endian.
    byte      --- Corresponds to (unsigned-byte 8)

The file header looks as follows:

    -------------------------------------------------------
    POS HEX THING
    -------------------------------------------------------
    00  89  
    01  50  ASCII P
    02  4c  ASCII L
    03  55  ASCII U
    04  4d  ASCII M
    05  50  ASCII P
    06  0d  ASCII CR
    07  0a  ASCII LF
    08  1a  DOS EOF
    09  0a  ASCII LF
    0a  01  ASCII SOH
    0b      1-byte version number
    0c      4-byte timestamp
    ..      ..
    10  02  ASCII STX
    -------------------------------------------------------

Following the header is a single chunk. Chunks are generally composed of a `type` header, followed by the content as specified by the chunk type. The `type` header is given here in the form of four ASCII characters, which are then byte-composed into a single `(unsigned-byte 32)` integer.

    -------------------------------------------------------
    TYPE  LISP-TYPE               LAYOUT
    -------------------------------------------------------
    NULL  NULL                    type 
    NODE  NODE                    type 
    NEST  NESTING-NODE            type children
    CHLD  CHILD-NODE              type
    TXND  TEXTUAL-NODE            type string
    ROOT  ROOT                    type children
    TEXT  TEXT-NODE               type string
    COMM  COMMENT                 type string
    DOCT  DOCTYPE                 type string
    ELEM  ELEMENT                 type string map children
    FTXT  FULLTEXT-ELEMENT        type string map children
    XMLH  XML-HEADER              type map
    CDAT  CDATA                   type string
    PROC  PROCESSING-INSTRUCTION  type string string
    -------------------------------------------------------

The footer looks like so:

    -------------------------------------------------------
    POS HEX THING
    -------------------------------------------------------
    00  04  ASCII ETX
    -------------------------------------------------------

Thus, a sample document as denoted in standard XML syntax

    <start><foo bar="baz">Hello!</foo></start>

Is transformed into the following bundle (as displayed by Emacs' hexl-mode)

    00000000: 8950 4c55 4d50 0d0a 1a0a 0101 d8a4 84c2  .PLUMP..........
    00000010: 0252 4f4f 5400 0000 0145 4c45 4d00 0000  .ROOT....ELEM...
    00000020: 0573 7461 7274 0000 0000 0000 0001 454c  .start........EL
    00000030: 454d 0000 0003 666f 6f00 0000 0100 0000  EM....foo.......
    00000040: 0362 6172 0000 0003 6261 7a00 0000 0154  .bar....baz....T
    00000050: 4558 5400 0000 0648 656c 6c6f 2104       EXT....Hello!.
