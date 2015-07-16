# cl-durian
cl-durian creates HTML from simple list structures.

```
(cl-durian:html `(html ((h1 "This is the title") (body ((p "line1")(p "line2"))))))

"<html><h1>This&nbsp;is&nbsp;the&nbsp;title</h1><body><p>line1</p><p>line2</p></body></html>"
```
# More Examples
### attributes
Attributes are handled as a list of lists like in the syntax of *let*.
```
(cl-durian:html `(tag ((att1 "val1") (att2 "val2")) "stuff in body"))

"<tag ATT1=\"val1\" ATT2=\"val2\">stuff&nbsp;in&nbsp;body</tag>"
```
Note that this is detected by the outer list having a length of 3. If you would like to have attributes, but no text between the tags, then use either the empty list (as *()* or *nil*) or the empty string *""*.
```
(cl-durian:html `(tag ((att1 "val1") (att2 "val2")) ""))

"<tag ATT1=\"val1\" ATT2=\"val2\"></tag>"
```
### tags within strings
Tags must be symbols, a list starting with anything other than a symbol is taken to be a list of things that must be handled separately.
```
(cl-durian:html `(p ("this has an " (i "italic") " word")))

"<p>this&nbsp;has&nbsp;an&nbsp;<i>italic</i>&nbsp;word</p>"
```
### scripts
The *script* symbol from the cl-durian package is used to denote a script, and thus the text within these tags will not be escaped as other html text is.
```
(cl-durian:html `(cl-durian:script ((type "text/javascript")) "var j = 10;"))

"<script TYPE=\"text/javascript\">var j = 10;</script>"
```
### raw text
The *raw* symbol from the cl-durian package is used to denote text that is note meant to be escaped and is not to be surrounded by tags.

This is meant to allow a way to avoid escaping if you are to want to insert CSS or HTML from another file without causing it to be escaped again.
### interpolation
List structures with the backtick are used to allow for interpolation.
```
(let ((name "George"))
  (cl-durian:html `(html ((h1 "NAME") (body (p ,name))))))

"<html><h1>NAME</h1><body><p>George</p></body></html>"
```
Functions that return list structures can be used to help abstract. They can then be called within other list structures.

*when* and list splicing can be used to conditionally add things into the html list structure.
```
(cl-durian:html `(html ,@(when user-identified `(,(user-banner session-value)))))
```
Functions that edit or wrap other list structures can also be used to great effect.
# Dictionary
### html
`html' takes a list structure and creates a formatted html string. This is the main function of cl-durian.
### \*force-tags-lowercase\*
defaults to *t* (true)

This forces all tags to lowercase. It can be changed, but it defaults to *t* for convenience as symbols are naturally read in all caps by the lisp reader.
Changing this to *nil* will allow control over the case of tags.
Note that as symbols are read in in capital letters, symbols will need to be put between |these| to get any |mIxEd| lowercase.
### html-escape
escapes a string for use in html (not generally needed, cl-durian handles the escaping of strings automatically).
### script
This symbol denotes that a script follows. Scripts are unescaped.
### raw
This is a symbol used by the package to denote that the text in the given list is untagged and needs no escaping.
