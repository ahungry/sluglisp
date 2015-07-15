# sluglisp

Web based index of Quicklisp projects

Currently only works for projects hosted on github.

I plan to add additional features, from popularity integration of
packages to some .asd file exports you will be able to generate via
checkboxes next to the package names.

## Usage

Visit the live demo at http://sluglisp.ahungry.com for now.

When the project is complete, plan to load via a simple:

```lisp
(ql:quickload :sluglisp)
(sluglisp:start :port 5000)
```

And then visit:

http://localhost:5000

For now (or then) scroll up and down the left pane and click on a
project to learn more about it.

## Installation

For now if you want to toy with the files, clone the repo:

```sh
git clone 'https://github.com/ahungry/sluglisp.git'
```

## Author

* Matthew Carter (m@ahungry.com)

## Copyright

Copyright (c) 2015 Matthew Carter (m@ahungry.com)

# License

Licensed under the AGPLv3 License.
