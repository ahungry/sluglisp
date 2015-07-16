Top-level ASDF packages:

 * cl-glfw - GLFW bindings for Common Lisp.
 * cl-glfw-opengl-version_* - OpenGL bindings for Common Lisp.
 * cl-glfw-glu (requires cl-glfw-opengl) - OpenGL Utilities bindings for Common Lisp.

ABOUT

A public domain set of CFFI bindings and convenience macros for the GLFW, GLU
and OpenGL libraries.

The OpenGL bindings are automatically generated from the spec-files from 
http://www.opengl.org/registry/

The GLU binding was hand-coerced from a tidied up header file, through swig, so
it's pretty much a direct mapping onto the API.

cl-glfw is independent of cl-glfw-opengl-* and cl-glfw-glu. cl-glfw-opengl and
cl-glfw-glu may be used without cl-glfw. cl-glfw-glu, however, does depend on
cl-glfw-opengl. So, you should be able to use cl-glfw and
cl-glfw-opengl/cl-glfw-glu independently of each other; for example, using
cl-glfw with cl-opengl, or cl-opengl's glut with cl-glfw-opengl, should work
fine.

Check out the examples/ directory for more of a guide through.


WINDOWS NOTES

For SBCL on Windows, you will need to download the GLFW DLLs pack
(glfw-2.6.bin.WIN32.zip), which you can find on http://glfw.sf.net/

I would recommend either placing the MINGW version of the GLFW.dll in either
your \WINDOWS\SYSTEM32 directory, or in the current directory for distributed
binaries.


MAC OS X NOTES

For SBCL on Mac OS X, you will want to have the libglfw.dylib somewhere where
cl-glfw can find it. It's probably easiest to just include it in the current
directory of your application.


THREADING

GLFW threading WILL PROBABLY BREAK YOUR LISP IMAGE. I would advise seeking
other avenues if you require threading in your applications. The bindings
remain in cl-glfw, but, I should emphasise once again, that they probably are
going to mess up things like garbage collection in your lisp, and apparently
some things to do with stacks and allocations aswell.


NAME MANGLING STYLE

All function/constant names are 'lispified', that is dash-separated, instead of
camel-case.  Suffixes and acronyms are kept together as one word. Library
prefixes are expressed as their package. Constants are surrounded by the '+'
characters as lisp convention dictates.  Some examples:

glfwOpenWindow -> glfw:open-window
glVertex3fv -> gl:vertex-3fv
gluBuild2DMipmaps -> glu:build-2d-mipmaps
GL_FOG_INDEX -> gl:+fog-index+
GL_LIGHT0 -> gl:+light-0+


TYPE CONVERSION IN CL-GLFW-OPENGL

Functions that take a predictable c-array input or return an output have
automatic-translators for lisp-sequences (array and vectors). However, this
will require an extra allocation. cffi:pointer types will be passed straight
through. Eg. (gl:vertex-3fv #(1.0 0.0 1.0)) will work as expected.

All function parameters of types (warning: not in GLU yet) GLfloat or GLdouble
are automatically wrapped in an appropriate coerce. All integer types are
expected to be acceptable to CFFI. 

See individual function documentation for cl-glfw conversion details.
cl-glfw-glu does not do automatic float translations.


PLATFORMS
    SBCL Linux x86
    SBCL Linux amd64/x86_64
    SBCL Windows 32-bit
    SBCL Mac OS X 32-bit Intel
    Others: Let me know.


LINKS

    http://repo.or.cz/w/cl-glfw.git - The working online git repository
    http://glfw.sf.net/ - The homepage for GLFW
    http://common-lisp.net/project/cffi/ - Dependency library CFFI's homepage
    http://www.sbcl.org/ - Our favoured implementation of Common Lisp
    http://www.opengl.org/ - OpenGL 3D graphics library API


ALTERNATIVES

    http://common-lisp.net/project/cl-opengl/ - Alternative set of OpenGL (with GLUT) bindings
    http://cl-sdl.sf.net/ - Seemingly abandoned SDL and OpenGL bindings


Enjoy.
Bill
airbaggins@users.sourceforge.net

