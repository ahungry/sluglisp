# fs-watcher

## Description

Watches for filesystem changes to act upon them.

## Documentation

There is not much to this library. Only a single function.

### Installation

    (ql:quickload 'fs-watcher)

### The watch function

This is the signature of the function:

    (fs-watcher:watch pathnames callback &key (delay 1))

- `pathnames` is a pathname or a list of pathnames. Both are accepted.
- `callback` is a callback function called on each change.
- `:delay` is the looping delay.

At first, the function will get the list of all pathnames. For example,
if you give a directory pathname, `fs-watcher` will recursively look for
files and directories in it.

Once it has all the necessary information, it constantly polls for them
to see if there is any change. This polling is done every second by default.

Whenever there is a change, the `callback` is called with an argument:
the pathname that changed.

### Usage example

    (fs-watcher:watch "/home/user/code/" #'(lambda (pathname)
                                             (inferior-shell:run/s "make")))

## Contributors

- [Florian Margaine](http://margaine.com)

## License

MIT License.
