# chrome-native-messaging

Communicate with Chromium or Google Chrome extensions, as a native
application.

For more information on what I mean, please look at
[nativeMessaging][0] documentation.

## API

This package provides 2 functions: `read-from-ext` and `send-to-ext`.

### `read-from-ext`

```lisp
(defun read-from-ext (stream)
  "Reads the string sent from the extension. The stream is usually *standard-input*.")
```

A typical call to this function looks like this (using the `jsown`
package to parse the JSON):

```lisp
(let* ((buffer (chromium-native-messaging:read-from-ext *standard-input*))
       (json-object (jsown:parse buffer)))
  ; code
  )
```

### `send-to-ext`

```lisp
(defun send-to-ext (string output-stream)
  "Sends a string to the extension. Usually, output-stream is *standard-output*.")
```

A typical call looks like this (again, using the `jsown` package):

```lisp
(chrome-native-messaging:send-to-ext
 (jsown:to-json
  (jsown:new-js
    ("foo" "bar")))
 *standard-output*)
```

## License

MIT license.


  [0]: https://developer.chrome.com/extensions/nativeMessaging
