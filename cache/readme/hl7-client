# hl7-client

Common-Lisp client for sending HL7-Messages with MLLP envelope over TCP/IP socket.

> The HL7 Version 2 Messaging Standard — Application Protocol for Electronic
> Data Exchange in Healthcare Environments — is considered to be the workhorse
> of data exchange in healthcare and is the most widely implemented standard
> for healthcare information in the world.

Source: http://www.hl7.org/about/FAQs/index.cfm?ref=nav ("General HL7 FAQ" > "Version 2" > "What is Version 2?")


The client sends HL7-Messages in MLLP evelopes to a TCP/IP socket and returns the acknowledges.

There is no validation of messages or acknowledges done by the client so it will NOT stop when an NACK-Message occurs.

## License
http://opensource.org/licenses/BSD-3-Clause

## Installation

HL7-Client is 'quickloadable':
```cl
(ql:quickload "hl7-client")
```


## Usage

```cl
(hl7-client:send server port message-or-message-list)
```
* server: hostname [String]
* port: receiving port of server [Number]
* message-or-message-list: single Message [String] or [List] of Messages

hl7-client:send returns a list with the HL7-acknowledges

## Example

For testing purposes there is a function (hl7-client:get-hl7-test-message) which returns a HL7-Message with random message-ID.

```cl
CL-USER> (hl7-client:SEND "localhost" 24777 (hl7-client:get-hl7-test-message))
("MSH|^~\\&|Receiving-App^^|Receiving-Facility^^|Sending-App^^|Sending-Facility^^|20150101195400^||ACK|9089|P^|2.2^^|||||||^^^^^|^MMSA|AA|9089^M")
CL-USER> (hl7-client:SEND "localhost" 24777 (list (hl7-client:get-hl7-test-message) (hl7-client:get-hl7-test-message)))
("MSH|^~\\&|Receiving-App^^|Receiving-Facility^^|Sending-App^^|Sending-Facility^^|20150101195400^||ACK|1800|P^|2.2^^|||||||^^^^^|^MMSA|AA|1800^M"
 "MSH|^~\\&|Receiving-App^^|Receiving-Facility^^|Sending-App^^|Sending-Facility^^|20150101195400^||ACK|1309|P^|2.2^^|||||||^^^^^|^MMSA|AA|1309^M")
CL-USER> 
```

^M = #\Return

## Dependencies
* usocket

Installable via Quicklisp:

```cl
(ql:quickload "usocket")
```


## Development
I developed and tested hl7-client on:
* Debian GNU/Linux (jessie)
* SBCL and CCL

## ToDo
* 'with'-Macro for sending multiple messages within one connection

