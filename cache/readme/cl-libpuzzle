# Cl-Libpuzzle

libpuzzle binding for Common Lisp

## Usage

cl-libpuzzle provides libpuzzle FFI interface.

### Extra

Provide macro interface for easy use.

    (with-context-cvecs ctx (cv1 cv2)
     (puzzle-fill-cvec-from-file ctx cv1 "/path/to/your/file1")
     (puzzle-fill-cvec-from-file ctx cv2 "/path/to/your/file2")
     (puzzle-vector-normalized-distance ctx cv1 cv2 1))

    =>

    (CFFI:WITH-FOREIGN-OBJECTS ((CTX 'CL-LIBPUZZLE::PUZZLE-CONTEXT)
                                (CV1 'CL-LIBPUZZLE::PUZZLE-CVEC)
                                (CV2 'CL-LIBPUZZLE::PUZZLE-CVEC))
     (UNWIND-PROTECT
      (PROGN
       (PUZZLE-INIT-CONTEXT CTX)
       (PUZZLE-INIT-CVEC CTX CV1)
       (PUZZLE-INIT-CVEC CTX CV2)
       (PUZZLE-FILL-CVEC-FROM-FILE CTX CV1
        "/path/to/your/file1")
       (PUZZLE-FILL-CVEC-FROM-FILE CTX CV2
        "/path/to/your/file2")
       (PUZZLE-VECTOR-NORMALIZED-DISTANCE CTX CV1 CV2 1))
      (PUZZLE-FREE-CVEC CTX CV1)
      (PUZZLE-FREE-CVEC CTX CV2)
      (PUZZLE-FREE-CONTEXT CTX)))

## Author

* Masato Sogame (poketo7878@gmail.com)

## Copyright

Copyright (c) 2013 Masato Sogame (poketo7878@gmail.com)

