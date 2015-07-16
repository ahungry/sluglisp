glu-tessellate
==============
This is a triangulation library that wraps around [GLU's tessellation routines](http://www.glprogramming.com/red/chapter11.html)
to make turning vectors of point data into triangles stupid easy.

Yes, it supports concave polygons, and yes it supports holes. It also allows you to specify which winding
rule you use to triangulate.

It also returns every triangle it produces in counter-clockwise order. You can change this to
clockwise by passing `:cw t`.

Usage
-----
The library has one public function (don't you love simplicity?) which takes a vector points (a point
being an '(x y) list) into a list of triangles:

	;; returns two triangles making up this square
    (glu-tessellate:tessellate #( (0 0) (0 10) (10 10) (10 0) ))  ->
        (((10.0d0 10.0d0) (0.0d0 10.0d0) (0.0d0 0.0d0))
		 ((10.0d0 0.0d0) (10.0d0 10.0d0) (0.0d0 0.0d0)))
    
It also supports polygons with holes. Holes are specified as a list of point vectors:

	;; a square with a hole returns quite a few more triangles
    (glu-tessellate:tessellate #( (0 0) (0 10) (10 10) (10 0) ) :holes '( #((3 3) (3 7) (7 7) (7 3)) )) ->
        (((10.0d0 10.0d0) (0.0d0 10.0d0) (3.0d0 7.0d0))
         ((0.0d0 0.0d0) (3.0d0 7.0d0) (0.0d0 10.0d0))
         ((3.0d0 7.0d0) (0.0d0 0.0d0) (3.0d0 3.0d0))
         ((10.0d0 0.0d0) (3.0d0 3.0d0) (0.0d0 0.0d0))
         ((3.0d0 3.0d0) (10.0d0 0.0d0) (7.0d0 3.0d0))
         ((10.0d0 10.0d0) (7.0d0 3.0d0) (10.0d0 0.0d0))
         ((7.0d0 3.0d0) (10.0d0 10.0d0) (7.0d0 7.0d0))
         ((3.0d0 7.0d0) (7.0d0 7.0d0) (10.0d0 10.0d0)))

### Arguments
The one function this library uses takes the following keyword arguments:

    :holes           ; a list of point vectors describing the holes in the polygon
	:winding-rule    ; a keyword describing the winding rule: (:odd :nonzero :positive :negative :abs-geq-two)
	:cw              ; if T, produced triangles will be in CW order instead of CCW

Limitations
-----------
GLU tessellation normally allows you to store arbitrary data with each point and also merge
this point data when new points are created. This library doesn't currently allow this, and is
only used for triangulation of points. If you need to attach colors to your triangles, it must
be done so outside of the library...unless, of course, you wish to add arbitrary data support
and issue a pull request ;).

The only other limitation I can think of is that it relies on GLU being on the system. Show me
a system without GLU and I'll show you a computer from an alternate universe where Microsoft
finally did away with all competition and DirectX rules o'er the land with an iron fist.
