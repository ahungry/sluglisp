# Marching-Cubes

A marching cubes algorithm implementation in Common Lisp based on Paul Bourke's (http://paulbourke.net/geometry/polygonise/)

## API

### [Function] marching-cubes

    MARCHING-CUBES density-function min-position max-position delta isolevel => triangles

Extracts a surface from density field represented with `density-function` and returns `triangles` which is a list of triangles. `density-function` is a function that takes three arguments representing the position of a point and returns density as a scalar at the point. `min-position` and `max-position` are `vec3` values and  specify the minimun point and the maximun point of a grid to be processed. `delta` is a scalar that specifies the size of a cell in a grid to be processed. `isolevel` is a scalar that specifies the threshold of mesh construction.

### [Function] marching-cubes-smooth

    MARCHING-CUBES-SMOOTH density-function normal-function min-position max-position delta isolevel => smooth-triangles

Same as `marching-cubes` except that `marching-cubes-smooth` returns a list of smooth triangles which have normal vectors at each vertex. `normal-function` is a function that takes three arguments representing the position of a point and returns a normal vector at the point. Other arguments are same as `marching-cubes`'s. 

## Example

See example/ directory.

## Author

* Masayuki Takagi (kamonama@gmail.com)

## Copyright

Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)

## License

Licensed under the LLGPL License.

