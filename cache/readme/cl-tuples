# cl-tuples - A set of macros for auto-generating optimised vector math routines

## A tuple type declaration auto-generates a number of useful functions, macros, and types. 

It is best to give an example.

	(def-tuple-type vector2d
		:tuple-element-type short-float
		:initial-element 0.0f0
		:elements (x y))

Will declare a tuple of short-floats, arrays of which are initialised
with the element 0.0f0 and which has two elements, named x and y.

There will be a struct to represent this type, declared as follows:

	(defstruct vector2d
		:type vector
		:constructor nil
		(x 0.0f0 :type short-float)
		(y 0.0f0 :type short-float))

i.e. a struct, stored as a vector with elements representing the
elements of the tuple, initialised to the initial-element value of the
tuple.

Literals can be written via the modified read syntax

		#[ vector2d 0.2 1.2 ] => #( 0.2 1.2 )
		#[ vector2d* 0.2 1.2 ] => (values 0.2 1.2)

It is reccomended literals are written with the above syntax as their
expansion will also incorportate type definitions that will be
compatible with the following routines that will be generated to be
able to manipulate them.

	(vector2d-values* x y) => (values x y)          ;; convert from args to values
	(vector2d* v) => (values (aref v 0) (aref v 1)) ;; covert from array to values
	(new-vector2d)                                  ;; returns an empty tuple vector- i.e. #( 0 0 )
	(make-vector2d x y)                             ;; returns a vector (struct) as #( x y )
	(make-vector2d* (values x y))                   ;; same as the above only with multiple value arguments
	(setf (vector2d* v) (values x y) )              ;; generalised set that takes multiple values
	(with-vector2d v (i j) ...)                     ;; binds x and y of tuple vector v to i and j in the body
	(with-vector2d* (values x y) (i j) ..)          ;; same as the above, only it expects a values form

	;; arrays -- this can create an array  of n vector2ds (eg 4 vector2ds == 8 element array)								
	(make-vector2d-array dimensons &key adjustable fill-pointer)

	(vector2d-aref v  n)  						    ;; treats v as an array of n vector2d's and
												    ;; returns the nth vector2d as a vector (ie
												    ;; struct)
	(vector2d-aref* v n)							;; treats v as an array of n vector2d's and returns the 
												    ;; nth vector2 as multiple values
                       
	(setf (vector2d-aref v n) #( x y ))             ;; sets the n'tn vector2d in the array v
	 
	(setf (vector2d-aref v n) (values x y ))       	;; sets the n'tn vector2d in the array v, expects multiple
													;; value argument
		
     (vector2d-push #( x y ) v)                     ;; push an vector2d into an array of vector2d
	 (vector2d-push*  (values x y) v)               ;; same as above but with multiple values
	 (vector2d-push-extend #( x y ) v)              ;; as vector2d-push but admits the possiblity of extension
	 (vector2d-push-extend* (values x y) v)         ;; same as above but takes multiple value arguments

	(vector2d-fill-pointer v)                       ;; returns fill pointer 
	(setf (vector2d-fill-pointer v) x)              ;; sets fill pointer
	(vector2d-array-dimensions v)                   ;; returns number of vector2d's array can hold

In addition a small convienince reader syntax is implemented - #{ x y
z } is equivalent to (values x y z) as client code of this library is
likely to manipulate many multiple values.

Note that the code cl-tuples generates is implementation agnostic: it
is heavily predicated on the assumption that your implementation does
a good job of optimising multiple value calls. If this is not the
case, then the convienence of the array - related functions are
probably the only good reason to use this library.

## HOWTO

A two-dimensional vector value is created by `MAKE-VECTOR2D`:

    > (make-vector2d 1f0 1f0)
    #(1.0 1.0)

The type `FAST-FLOAT`, which is used for all float values, is actually a
subtype of `SINGLE-FLOAT`, so make sure to only use values that fit into
that type.

To calculate the length of this vector `VECTOR2D-LENGTH*` can now be
used like this:

    > (let ((v (make-vector2d 1f0 1f0)))
        (vector2d-length* (vector2d* v)))
    1.4142135

By converting the object into a bunch of variables, the macro pipeline
keeps transient objects and function calls away.  The above form thus
expands to something like the following (type declarations and some
other code omitted for clarity):

    (LET ((V (MAKE-VECTOR2D 1.0 1.0)))
      (MULTIPLE-VALUE-BIND (#:G1764 #:G1765)
          (VALUES (AREF V 0) (AREF V 1))
        (SYMBOL-MACROLET ((X #:G1764) (Y #:G1765))
          (SQRT (+ (* X X) (* Y Y))))))

The coordinates of the vector are bound and made available to the length
calculation code.  If we skip the object creation and go straight the
`VALUES` route, the following is approximately the same as above,
without ever creating a vector object.

    > (vector2d-length* (vector2d-values 1.0 1.0))
    1.4142135

The reader syntax may be used to the same effect:

    > (enable-tuples-syntax)
    > #{1.0 1.0}
    1.0
    1.0
    > (vector2d-length* #{1.0 1.0})
    1.4142135

(Since the reader syntax and `VECTOR2D-VALUES` expand directly into a
`VALUES` call, nothing prevents you from using that as well.)

Based on this design more operations are implemented.  See the API and
the tests for details on vectors, vertexes, matrixes and quaternions.

Defining new operators is done via `DEF-TUPLE-OP`, e.g.:

    (def-tuple-op scaling-matrix44*
        ((sx fast-float)
         (sy fast-float)
         (sz fast-float))
      (:return matrix44
               (matrix44-values*
                sx    0.0f0 0.0f0 0.0f0
                0.0f0 sy    0.0f0 0.0f0
                0.0f0 0.0f0 sz    0.0f0
                0.0f0 0.0f0 0.0f0 1.0f0)))

This operator accepts three arguments and creates the obvious matrix
from them.  So lets say, a function has as a conventional argument a
vector of three elements.  Binding each element to a name and applying
the above operator to them gives us the following:

    > (let ((v (make-vector3d* #{1f0 1f0 1f0))))
        (with-vector3d v (sx sy sz)
          (make-matrix44* (scaling-matrix44* sx sy sz)))
    #(1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0)

The calculated matrix is converted to an actual object to be returned.

## ASSORTED EXAMPLES

    > (let ((v (make-vector2d 1f0 1f0))
            (s 2f0))
        (vector2d-length* (vector2d-scale* (vector2d* v) s)))
    2.828427

# QUATERNIONS

(Adapted from the documentation of cl-quaternion to this API.)

Creating a quaternion from real and imaginary components.  The first
argument is the real part, and the rest are the imaginary components.

    > (make-quaternion* (quaternion-values* 10f0 3f0 0f0 0f0))
    #(10.0 3.0 0.0 0.0)

Quaternions can be normalized and magnitudes may be computed.

    > (make-quaternion* (quaternion-normalize* (quaternion* *)))
    #(0.9578263 0.28734788 0.0 0.0)
    > (quaternion-mag* (quaternion* *))
    1.0

Quaternion addition and multiplication are supported.

    > (make-quaternion*
       (quaternion-sum* (quaternion-values* 3f0 0f0 0f0 0f0)
                        (quaternion-values* 1f0 1f0 0f0 1f0)))
    #(4.0 1.0 0.0 1.0)
    > (make-quaternion*
       (quaternion-product* (quaternion-values* 3f0 0f0 0f0 0f0)
                            (quaternion-values* 1f0 1f0 0f0 1f0)))
    #(3.0 0.0 3.0 -3.0)

Unit quaternions may be used to represent rotations.  Functions are
provided for working with quaternions for this purpose.

    > (values fast-pi (type-of fast-pi))
    3.1415927
    SINGLE-FLOAT
    > (make-quaternion*
       (angle-axis-quaternion*
        (angle-axis-values* 0f0 0f0 1f0 (/ single-pi 2f0))))
    #(0.0 0.0 0.70710677 0.70710677)

Vectors can then be transformed using these quaternions.

    > (quaternion-transform-vector3d*
       (vector3d-values* 0.0 1.0 0.0)
       (angle-axis-quaternion*
        (angle-axis-values* 0.0 0.0 1.0 (/ fast-pi 2))))
    -0.99999994
    0.0
    0.0

At the moment you have still to convert an angle-axis representation to
either a matrix or a quaternion by yourself to rotate a vector by it.

    > (quaternion-transform-vector3d*
       (vector3d-values* 0.0 1.0 0.0)
       (angle-axis-quaternion*
        (angle-axis-values* 0.0 0.0 1.0 fast-pi)))
    8.742278e-8
    -1.0
    0.0
    > (transform-vector3d*
       (angle-axis-matrix33*
        (angle-axis-values* 0.0 0.0 1.0 fast-pi))
       (vector3d-values* 0.0 1.0 0.0))
    8.742278e-8
    -1.0
    0.0
