Encoding
----------
(**URLENCODE** string &key (**queryp** nil)) → string

**queryp** : If true you get application/x-www-form-urlencoded.

Decoding
----------
(**URLDECODE** string &key (**queryp** nil) (**eol** :LF) (**lenientp** nil)) → string

**queryp** : If true you get application/x-www-form-urlencoded.  
**eol** : The line ending to use, possible values are (:CR :LF :CRLF).  
**lenientp** : If true unencoded reserverd characters will appear unmodified in the result instead of causing a condition to be signaled.

**URLENCODE-MALFORMED-STRING** will be signaled if the string is not a valid urlencoded string.
