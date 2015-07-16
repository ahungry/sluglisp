cl-apple-plist
==============

Encodes Common Lisp data structures in Apple property list XML format (.plist).

Released under a BSD style license.

Introduction
------------

When building Apple IOS applications, it's convenient to use .plist files to store reference data.

[About Property Lists](http://developer.apple.com/library/mac/#documentation/Cocoa/Conceptual/PropertyLists/AboutPropertyLists/AboutPropertyLists.html) 

Rather than maintain plist files by hand or using Xcode, I've found it useful to create Lisp
data structures as s-expressions and then use this library to export to .plist files.

Notes
------

Lists are encoded as array tags, number as integer or real and t and nil map to true and false respectively.

	CL-USER> (encode-apple-plist (list "hello" "world" 1 2.89 t nil))
	<?xml version="1.0" encoding="UTF-8"?>
	<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
	<plist version="1.0">
	<array>
	<string>hello</string>
	<string>world</string>
	<integer>1</integer>
	<real>2.89</real>
	<true/>
	<false/>
	</array>
	</plist>
	NIL

Hash tables become dict tags.


     CL-USER> (let ((h (make-hash-table))) (setf (gethash 1 h) "one") (encode-apple-plist h))
     <?xml version="1.0" encoding="UTF-8"?>
     <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
     <plist version="1.0">
     <dict>
     <key>1</key><string>one</string>
     </dict>
     </plist>
     NIL

encode-apple-plist takes an optional stream argument, but encode-apple-plist-to-file(data filename) is a convenenience function that writes the plist
to a file.

    CL-USER> (encode-apple-plist-to-file ((list "1" "2") "myfile.plist"))


Strings are HTML-encoded using the html-encode library.

There is currently no support for date tags, partly because Lisp dates are stored as integers rather than arecognizable date type. Ideas welcome.

There is currently no support for data tags. Ideas welcome.

Should BITs be mapped to true and false or 0 and 1?

Rob Blackwell

October 2011 