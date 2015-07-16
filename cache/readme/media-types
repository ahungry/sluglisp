MEDIA-TYPES is a library for querying and comparing media types.

Probably the most useful function is `extension-media-type`:

    (extension-media-type "txt") => "text/plain"

An unknown extension, or no extension, has a media type of
`application/octet-stream`:

    (extension-media-type "my-fake-extension") => "application/octet-stream"
    (extension-media-type nil) => "application/octet-stream"

(This is nonstandard, but reflects general practice.)

You can also use `media-type-extension` to query the extension (or
extensions) for a media type:

    (media-type-extension "application/javascript") => "js"

If a media type has multiple extensions, they are returned as multiple
values:

    (media-type-extension "image/jpeg") => "jpeg", "jpg", "jpe"

Extensions are not case sensitive:

    (extension-media-type "TXT") => "text/plain"

You may pass the extension with or without a leading dot:

    (extension-media-type ".txt") => "text/plain"

You can get the extension of a file name with `file-name-extension`:

    (file-name-extension "file.txt") => "txt"

Or just get the media type of the file directly:

    (file-name-media-type "file.txt") => "text/plain"
    (file-name-media-type #p"file.txt") => "text/plain"

A file name without an extension has a media type of `application/octet-stream`:

    (file-name-media-type "file") => "application/octet-stream"

However, if you want to *test* whether a file is of a specific media
type, you should use `file-name-media-typep`.

    (file-name-media-typep "file.xml" "application/xml") => t
    (file-name-media-typep "file.svg" "application/xml") => t

`file-name-media-typep` uses `media-subtypep`, which has a
sophisticated understanding of media types:

    (media-subtypep "text/plain" "*/*") => t
    (media-subtypep "text/plain" "text/*") => t

    ;; All media types are effectively subtypes of application/octet-stream.
    (media-subtypep "text/plain" "application/octet-stream") => t

    ;; Structured syntax suffixes.
    (media-subtypep "image/svg+xml" "application/xml") => t
    (media-subtypep "application/vnd.my-format+json" "application/json") => t

    ;; Media type parameters.
    (media-subtypep "application/atom+xml;type=entry" "application/xml") => t
    (media-subtypep "application/atom+xml" "application/atom+xml;type=entry") => nil
    (media-subtypep "application/atom+xml;charset=UTF8;type=entry"
                    "application/atom+xml;type=entry")
    => t

    ;; The "q" pseudo-parameter is used in HTTP content negotiation.
    (media-subtypep "application/json" "*/*;q=0.1") => t

MEDIA-TYPES uses the `mime.types` from the Apache sources.
