# GCM

A [Google Cloud Messaging](http://developer.android.com/google/gcm/index.html) library for Common Lisp.

# Example

```lisp
CL-USER> (gcm:configure "<your api key>")
; No value
CL-USER> (gcm:send (make-instance 'gcm:message
                                  :payload (list "title" "Hello"
                                                 "text" "Hey there"))
                   :registration-ids (list "<a registration id>"))
#<GCM:RESPONSE {100A3022A3}>
```

# License

MIT
