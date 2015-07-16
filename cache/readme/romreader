# romreader
## An extensible reader of ROM formats in portable CL.

### Install
You are strongly encouraged to use this library via [Quicklisp](http://quicklisp.org/). Simply start your lisp and run: ```(ql:quickload 'romreader)```.

### Getting Started
If you're reading this, you're probably interested in parsing ROMs using Lisp. There are 3 things you can do with romreader: parse a rom it [supports](http://redlinernotes.com/docs/romreader.html#*valid-formats*_vars), teach it how to parse a new format, and access parts of a loaded rom.

* Parsing a supported format is as easy as calling ```(load-rom "path/to/my/rom.format")```.
* Teaching romreader to parse a new rom is done using [```defreader```](http://redlinernotes.com/docs/romreader.html#defreader_func). For an example, see [the NES reader](https://github.com/redline6561/romreader/blob/master/src/nes.lisp#L96).
* Accessing [slots of a rom instance](http://redlinernotes.com/docs/romreader.html#rom_class) is done with ```rom-metadata```, ```rom-binary```, and ```rom-format```.

Thus far, I have been using a plist to store metadata and a vector to store the binary but feel free to use your own representations. ```rom-binary``` and ```rom-metadata``` don't care. :)

### Docs
[API Docs](http://redlinernotes.com/docs/romreader.html)
