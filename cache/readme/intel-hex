# Intel-hex - A library to handle Intel HEX format.

[![Build Status](https://travis-ci.org/takagi/intel-hex.svg?branch=master)](https://travis-ci.org/takagi/intel-hex)
[![Coverage Status](https://coveralls.io/repos/takagi/intel-hex/badge.svg?branch=master)](https://coveralls.io/r/takagi/intel-hex?branch=master)

Intel-hex is a library for Common Lisp to handle Intel HEX format, which is a file format that conveys binary information in ASCII text form.

## Usage

This example shows an Intel HEX file that has four data records followed by an end-of-file record.

    :10010000214601360121470136007EFE09D2190140
    :100110002146017E17C20001FF5F16002148011928
    :10012000194E79234623965778239EDA3F01B2CAA7
    :100130003F0156702B5E712B722B732146013421C7
    :00000001FF

To read the file, just call READ-HEX-FROM-FILE function as following and get a byte array.

    (intel-hex:read-hex-from-file 512 "/path/to/hex-file.hex")
    => #(0 0 0 ... #x21 #x46 #x01 ...)

## Installation

You can install via Quicklisp.

    (ql:quickload :intel-hex)

## API

### [Function] read-hex

    READ-HEX size stream => byte-array

Reads Intel HEX format from STREAM and returns an array of bytes whose only dimension is SIZE. STREAM should be a character input stream.

### [Function] read-hex-from-file

    READ-HEX-FROM-FILE size filename => byte-array

Reads Intel HEX format from a file named FILENAME and returns an array of bytes whose only dimension is SIZE.

### [Function] read-hex-from-string

    READ-HEX-FROM-STRING size string => byte-array

Reads Intel HEX format from STRING and returns an array of bytes whose only dimension is SIZE.

## Record Types

There are fix record types defined in Intel HEX format and their implementation status in the library is as following.

Hex code | Record type | Status
---------|-------------|-------
'00' | Data Record | done.
'01' | End Of File Record | done.
'02' | Extended Segment Address Record | not implemented.
'03' | Start Segment Address Record | not implemented.
'04' | Extended Linear Address Record | done.
'05' | Start Linear Address Record | not implemented.

## Author

* Masayuki Takagi (kamonama@gmail.com)

## Copyright

Copyright (c) 2015 Masayuki Takagi (kamonama@gmail.com)

# License

Licensed under the MIT License.
