## Simple Bin Packing / Rectangle Packing

This code implements a simple algorithm for a complicated problem,
given a set of rectangles, pack them into a square/rectangle.

I wrote this because I am playing with OpenGL and want to pack
multiple graphics into a texture.

### Inspiration

This code is a straight adaptation of the code found
[Packing Lightmaps](http://www.blackpawn.com/texts/lightmaps/).


### Interface

The main interface consists of two functions `pack-rectangles` and
`pack-rectangles-tree`.
Both functions take two arguments,

1. A list of rectangles
2. A keyword argument `:size` specifying the size of the target rectangle
   (as a rectangle)

The only difference is the return value.
The function `pack-rectangles-tree` return a tree representation
of the packing.  This tree is mostly usefull as an intermediate representation.

The `pack-rectangles` returns a list of placements, each placement is
of the form `(x y orientation rectangle)` where `rectangle` is one of
the input rectangles, the `x` and `y` are the placement coordinates, and
`orientation` indicates if the rectangle is rotated.

At the moment `orientation` is always `:0` indicating no rotation.

### Rectangles

A rectangle is any list as along as the first two values of the list indicate
the width and height of the rectangle.  So for example
`(200 300 "This is a rectangle")` is a rectangle, and so is `(200 300)` or `(200 300 (lambda (x) x))`

### Supporting functions

1. `tree-utilized-size` takes one argument, a tree, and returns the packed size.
2. `rectangle-tree-to-rectangle-list` takes a tree as an argument and returns the same output as `pack-rectangles`.
3. `write-html` takes a tree and a file name and write a html file which will show the packing.

### Tips

The code tries to place the rectangles in the order they are given.  It turns out that
it pays off to trie them in order of size.  E.g. first sort them on either area or lexicographically
on the coordinates.

### Example

Simple packing.

```common-lisp
> (pack-rectangles (list '(100 200 "Hallo") '(300 400 "Nr 2")  '(50 100 "Tall") '(200 30 "Wide")))
=>
((400 0 :|0| 200 30 "Wide") (100 0 :|0| 300 400 "Nr 2")
 (0 200 :|0| 50 100 "Tall") (0 0 :|0| 100 200 "Hallo"))
```

Or using the tree

```common-lisp
> (pack-rectangles-tree (list '(100 200 "Hallo") '(300 400 "Nr 2")  '(50 100 "Tall") '(200 30 "Wide")))
=>
#<NODE {100CCF8473}>
> (write-html * "/tmp/pack.html")
=>
NIL
```

The resulting html shows a picture like this,
note that the target size was very large, so red rectangle
sticking out to the right easily fits.

![Screen shot of browser](sample.png)


### Improvements

1. It should have the ability to automatically increase the target size.
2. It should also try rotations.
3. Performance, it is currently not optimized for performance.  If
   performance is an issue, there is a lot that can be done to make it
   faster. 

### LICENSE

It is licensed under LLGPL.  This is because it is my default license
when I do not want to think about licensing stuff.  However, I am
flexible and if you want any other license, ask me, and I am likely to
agree with it.
