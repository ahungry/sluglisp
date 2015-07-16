L-MATH
======

L-MATH is a library for performing simple linear algebra. Vector and
matrix classes are available, as are simple linear interpolation
functions, spline-based interpolations (Catmull-Rom and B-Spline
methods), and various operations related to creating rotation matrices.

L-MATH also contains various spline implementations, including those
implemented using matrices:

* Hermite curves
* Cubic Beziér curves
* Uniform, non-rational B-Splines
* Catmull-Rom splines

There are also general splines that are calculated using recurrence
formulae (which is generally a more stable approach than using
matrices), such as:

* General Beziér curves of arbitrary degree
* Non-Rational B-Splines, including uniform and non-uniform
  parametrisations.

In addition, L-MATH contains various functions for creating random
data, including:

* uniformly distributed random numbers and vectors
* normal distribution of random numbers and vectors
* Perlin noise.

Vector
------

Vectors can be constructed using the VECTOR and TO-VECTOR
functions. VECTOR accepts a list of elements, like so:

```lisp
(lm:vector 1 2 3) => #<L-MATH:VECTOR 1.000 2.000 3.000 >
```

The VECTOR's dimension is defined by the number of elements in the
VECTOR function's lambda list.

```lisp
(lm:dimension (lm:vector 1 2 3 4)) => 4
```

TO-VECTOR is intended to transform other types into the VECTOR
type. At the moment it supports transforming lists and, trivially,
other vector objects:

```lisp
(lm:to-vector (list 1 2 3)) => #<L-MATH:VECTOR 1.000 2.000 3.000 >
```

Importantly, TO-VECTOR allows the vector's length to be modified:

```lisp
(lm:to-vector (list 1 2 3) :dimension 2) => #<L-MATH:VECTOR 1.000 2.000 >
(lm:to-vector (lm:vector 1 2) :dimension 3) => #<L-MATH:VECTOR 1.000 2.000 0.000 >
```

Vectors can typically be represented as lists. For instance:

```lisp
(lm:dot-product (lm:vector 1 0 1) (list 0 1 0)) => 0
```

Various operations (listed below) are available for VECTOR
objects. Many of these functions will also accept lists as VECTOR
representations.

```lisp
(lm:dimension VECTOR)
```
Returns the VECTOR's dimension.

```lisp
(lm:length VECTOR)
```
Synonym for DIMENSION.

```lisp
(lm:norm VECTOR)
```
Returns the VECTOR's length.

```lisp
(lm:vector= LHS RHS)
```
Returns T iff the two vectors are equal. Internally, the VECTOR class
stores the data as an array of double-floats. Because of rounding
errors it is not advisable to compare floating point values
exactly. VECTOR= uses the special variable *equivalence-tolerance* to
define the tolerance within which two vectors are considered
equal. *equivalence-tolerance* defaults to 0.0001, which should be
reasonable for most applications. For example:

```lisp
(lm:vector= (lm:vector 1 2 3) (lm:vector 1 2 3.1)) => NIL
(lm:vector= (lm:vector 1 2 3) (lm:vector 1 2 3.00001)) => T
```

```lisp
(lm:elt VECTOR index)
```
Returns the element at the given index. This is also a SETFable
place. VECTORs are zero based.

```lisp
(lm:x VECTOR) (lm:y VECTOR) (lm:z VECTOR) (lm:w VECTOR)
```
Returns the elements at indices 0, 1, 2 and 3 respectively. These are
all SETFable places.

```lisp
(lm:dot-product VECTOR)
```
Returns the VECTOR's dot product.

```lisp
(lm:cross-product LHS RHS)
```
Calculates the cross product between two 3-vectors.

```lisp
(lm:angle-between FROM-VECTOR TO-VECTOR)
```
Returns the angle, in radians, needed to align the FROM-VECTOR with
the TO-VECTOR. The angle is signed, and is a left-handed
rotation. Example:

```lisp
(lm:to-degrees (lm:angle-between (lm:vector 1 0) (lm:vector 0 1))) => 90.0d0
(lm:to-degrees (lm:angle-between (lm:vector 1 0) (lm:vector 0 -1))) => -90.0d0
```

```lisp
(lm:euclidean-distance LHS RHS)
```
Calculates the Euclidean distance between two vectors or two numbers.

Matrix
------

Matrices can be constructed using (lm:MAKE-MATRIX row col &key initial-elements)

```lisp
(lm:make-matrix 2 3 :initial-elements '(1 0 0
		    		        0 1 0))
=>					
#<L-MATH:MATRIX 2 x 3
1.000 0.000 0.000 
0.000 1.000 0.000 >
```

If :initial-elements isn't specified, the matrix elements are
initialised to zero.


```lisp
(lm:matrix= LHS RHS)
```
Ensures that two matrices are numerically equivalent. All the
real-valued components must be within *equivalence-tolerance* of each
other.

```lisp
(lm:matrix-rows MATRIX) (lm:matrix-cols MATRIX)
```
Returns the number of rows and columns in the matrix.

```lisp
(lm:matrix-elt MATRIX row col)
```
Returns the element at the given row and column. MATRIX objects are
zero based. This is a SETFable place.

```lisp
(lm:make-identity SIZE)
```
Returns a SIZE×SIZE identity matrix.

```lisp
(lm:roll-matrix SIZE ANGLE)
```
Returns SIZE×SIZE matrix which will rotate a post multiplied vector
around the z-axis. It is a left-handed rotation. The ANGLE is given in
radians. SIZE should be either 3 or 4.

```lisp
(lm:yaw-matrix SIZE ANGLE)
```
Returns SIZE×SIZE matrix which will rotate a post multiplied vector
around the y-axis. It is a left-handed rotation. The ANGLE is given in
radians. SIZE should be either 3 or 4.

```lisp
(lm:pitch-matrix SIZE ANGLE)
```
Returns SIZE×SIZE matrix which will rotate a post multiplied vector
around the x-axis. It is a left-handed rotation. The ANGLE is given in
radians. SIZE should be either 3 or 4.

```lisp
(lm:set-rotation-naming-convention CONVENTION)
```
Rebinds YAW-MATRIX, PITCH-MATRIX, and ROLL-MATRIX to rotate around
different axes. The available conventions are provided in
*lm:\*rotation-naming-conventions**

```lisp
(lm:create-rotation-matrix VIEW RIGHT UP &optional (SIZE 3))
```
Creates a rotation matrix from three vectors. VIEW is the direction
that the resulting vector should be pointing along, UP is the direction
upwards. RIGHT is the vector orthogonal to this. Will return a
left-handed rotation matrix. SIZE is the size of the matrix, and
should be either 3 or 4.

```lisp
(lm:create-rotation-from-view VIEW WORLD-UP &optional (SIZE (length SIZE)))
```
Given a direction to look in (VIEW), and the direction that is
'upwards' in a given coordinate system, this function creates a
rotation matrix to translate into that coordinate system. This
rotation is left-handed. SIZE should be either 3 or 4. 

```lisp
(lm:create-rotation-from-view-to-view FROM-VIEW TO-VIEW WORLD-UP)
```
Creates a rotation matrix that will rotate the vector FROM-VIEW on to
the vector TO-VIEW, using WORLD-UP as the coordinate system's 'upward'
direction. This is a left-handed rotation. Example:

```lisp
(let ((rotation (lm:create-rotation-from-view-to-view (lm:vector 1 0 0) 
						      (lm:vector 0 1 0) 
						      (lm:vector 0 0 1))))
	   (lm:* rotation (lm:vector 1 0 0)))
=> #<L-MATH:VECTOR 0.000 1.000 0.000 >
```



Interpolation
-------------

```lisp
(lm:linear-interpolation START END T-VAL)
```
Given two vectors (START and END), and a real valued parameter
(T-VAL), this returns a vector between START and END. When T-VAL is
zero, this returns START. When T-VAL is 1, this returns END. Values
between 0 and 1 return vectors between START and END; values below
zero return vectors "before" START; values above 1 return vectors
"after" END. The value 0.5 returns the vector exactly between START
and END. Example:

```lisp
(lm:linear-interpolation (lm:vector -1 0) (lm:vector 1 0) 0.5)
=> #<L-MATH:VECTOR 0.000 0.000 >
(lm:linear-interpolation (lm:vector 0 0 0) (lm:vector 10 10 10) 2)
=> #<L-MATH:VECTOR 20.000 20.000 20.000 >
```

This method also accepts arbitrary numbers, and will interpolate
between them:

```lisp
(lm:linear-interpolation 0 100 0.5) => 50.0
```

```lisp
(lm:between START END)
```
Returns the vector exactly between the VECTORs START and END.


General Operations
------------------

```lisp
(lm:equivalent LHS RHS)
```
Returns t iff the two objects are numerically equivalent. Numbers are
tested using =. Real-valued objects (REAL types, VECTORs and MATRIXs)
are compared to each other using a tolerance
*equivalence-tolerance*. VECTORs and MATRIX objects are compared using
VECTOR= and MATRIX=.

```lisp
(lm:copy OBJECT)
```
Returns a copy of the given VECTOR, MATRIX or list.

```lisp
(lm:negate OBJECT) (lm:negate! OBJECT)
```
Returns the arithmetic inverse of the given object. NEGATE! does so
destructively. Example:

```lisp
(lm:negate (list 1 -2 3)) => (-1 2 -3)
```

```lisp
(lm:to-radians ANGLE) (lm:to-degrees ANGLE)
```
Converts from radians to degrees, and vice versa.

```lisp
(lm:test-dimensions LHS RHS)
```
Ensures that the two items have the same dimensions. The items may be
lists, vectors or matrices in most sensible combinations. This
function is useful when implementing your own operations between
vectors and matrices to ensure that their dimensions agree. If they do
not, a DIMENSION-ERROR condition is signalled.

Arithmetic operations
---------------------

All the general arithmetic operations are defined:

```lisp
* (lm:+ LHS RHS)
* (lm:- LHS RHS)
* (lm:- OBJECT)
* (lm:* LHS RHS)
* (lm:/ LHS RHS)
```

Conditions
----------

L-MATH-ERROR: A general condition from which all error conditions for
the package inherit.

DIMENSION-ERROR: This is signalled when an operation is requested on
objects whose dimensions are inappropriate.

ZERO-NORM-ERROR: This is signalled on operations which do not make
sense for vectors with zero norm.

OPERATION-NOT-SUPPORTED: This is signalled when an arithmetic
operation is requested on two objects for which the operation is not
supported. This should usually not occur, and probably should be
considered a bug if it does.

General Comments
----------------

Both VECTOR and MATRIX classes have load forms
(MAKE-LOAD-FORM). Internally, the data is stored as arrays of
DOUBLE-FLOAT values.

For those operations which deal with rotations, note that rotation
matrices should be post-multiplied by the vectors. The coordinate
system is left-handed, as are the rotations.

Supported Compilers
-------------------

L-MATH is known to work on SBCL 1.0.29 and 1.0.55. While it should
work on other compilers, this so far has not been tested. Please feel
free to send in reports of which compilers you've successfully run
this with, or to file bug reports where L-MATH is having problems.

Getting and Installing
----------------------

L-MATH is available from its [home page][2] at Common Lisp.net, or
from its [github repository][1]. L-MATH can be installed using
ASDF-INSTALL and QUICKLISP:

```lisp
(require 'asdf-install)
(asdf-install:install 'l-math)

(quicklisp:quickload 'l-math)
```

Reporting Bugs
--------------

Bugs can be reported to https://github.com/TheRiver/L-MATH. 

License
-------

See the file LICENSE for the licensing details. In brief, L-MATH is
licensed under the GPL, with additional permissions giving link
exceptions (aka the Classpath exception). Importantly for a Common
Lisp library, this exception allows you to use this GPLed library in
your application regardless of the licenses of the compiler and the
other libraries you are using (as long, of course, as you satisfy
those licenses).

Note that this does not remove the obligation that the rest of the GPL
places on you, such as supplying the source code of this library.


[1]: https://github.com/TheRiver/L-MATH "Github Repository"
[2]: http://www.common-lisp.net/projects/l-math/ "Common Lisp.net"
