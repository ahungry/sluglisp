About Trivial-Thumbnail
-----------------------
A tiny library to create thumbnails with [ImageMagick](http://www.imagemagick.org/). The binaries `convert` and `mogrify` need to be available on your system for this to work.

How To
------
The main function to use is `create`.

    (thumbnail:create #p"~/input.png" #p"~/input-thumb.png")
    => #p"~/input-thumb.png"

Of course it allows for different kinds of thumbnail generation with the `crop` argument:

    (thumbnail:create #p"~/input.png" #p"~/input-thumb.png" :crop :WIDTH)
    (thumbnail:create #p"~/input.png" #p"~/input-thumb.png" :crop :HEIGHT)
    (thumbnail:create #p"~/input.png" #p"~/input-thumb.png" :crop T)

Using `:WIDTH` means that the image is first scaled down preserving the aspect ratio to fit into the given height and then crops the remaining width to fit the given width. `:HEIGHT` works similarly. `T` just crops the image without any scaling.

This library uses ImageMagick mostly because it can handle gif animations properly, which a lot of other tools cannot. Processing animation is calculation-intensive though, so if you want to disable that (resulting in a static gif), you can pass `:preserve-gif NIL`.

Trivial-Thumbnail attempts to locate the necessary binaries automatically, searching the usual paths such as `/usr/bin`, `/usr/local/bin` and `C:/windows/system32/`, `C:/Program Files/ImageMagick*/`. If it fails to find a suitable binary, it throws a warning at startup. When the binary is somewhere else, but still within your `PATH`, it should still work properly. Otherwise you will have to set `*CONVERT-BIN*` and `*MOGRIFY-BIN*` yourself.
