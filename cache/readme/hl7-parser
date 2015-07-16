# Common Lisp HL7-Parser

HL7-Parser for HL7 Version 2 Messages (decode / encode) for Common Lisp.

> The HL7 Version 2 Messaging Standard — Application Protocol for Electronic
> Data Exchange in Healthcare Environments — is considered to be the workhorse
> of data exchange in healthcare and is the most widely implemented standard
> for healthcare information in the world.

Source: http://www.hl7.org/about/FAQs/index.cfm?ref=nav ("General HL7 FAQ" > "Version 2" > "What is Version 2?")

## License
http://opensource.org/licenses/BSD-3-Clause

## Implementation
SBCL and CCL on Debian GNU/Linux

## Installation

HL7-Parser is 'quickloadable':

```cl
(ql:quickload "hl7-parser")
```

## Usage
```cl
(defun decode (string-or-stream)
```
decode: Accepts a String or a Stream containing HL7-Messages

```cl
(defun encode (message-list &key (delimiter "|^~\\&") (message-delimiter nil))
```
encode:
 * `message-list` List of decoded HL7-Messages
 * `delimiter` for encoding of Message
 * `message-delimiter` if true then the encoded Messages ends with a `#\Newline` if nil then no `#\Newline` is appended (default nil).


```cl
(defun test-parser (in-file out-file &key (external-format :iso-8859-1) (output-delimiters "|^~\\&"))
```
test-parser: Read `in-file` (with HL7-messages) decode every line and then encode it and write it to `out-file`.

Use `diff in-file out-file` to check if the encode and decode of the messages work.

## Example
```cl
CL-USER> (hl7-parser:decode (format nil "MSH|^~~\\&|test|test^test|~cEVN||123~c~c" #\Return #\Return #\Newline ))
((:MESSAGE
  (:SEGMENT (:FIELD "MSH" "|^~\\&" "test" (:COMPONENT "test" "test") "")
   (:FIELD "EVN" "" "123") "")))

CL-USER> (hl7-parser:encode (hl7-parser:decode (format nil "MSH|^~~\\&|test|test^test|~cEVN||123~c~c" #\Return #\Return #\Newline )))
("MSH|^~\\&|test|test^test|^MEVN||123^M") ; ^M -> #\Return

CL-USER> (hl7-parser:encode (hl7-parser:decode (format nil "MSH|^~~\\&|test|test^test|~cEVN||123~c~c" #\Return #\Return #\Newline )) :message-delimiter t)
("MSH|^~\\&|test|test^test|^MEVN||123^M
") ; ^M -> #\Return
```

How to set Delimiters for encoding:
```cl
CL-USER> (hl7-parser:decode (format nil "MSH#^~~\\&#test#test^test#~c" #\Newline ))
((:MESSAGE (:FIELD "MSH" "#^~\\&" "test" (:COMPONENT "test" "test") "")))

CL-USER> (hl7-parser:encode (hl7-parser:decode (format nil "MSH|^~~\\&|test|test^test|~c" #\Newline )) :delimiter "#^~\\&")
("MSH#^~\\&#test#test^test#")
```

Pay attention to the character escaping (~ in format directive and backslashes)