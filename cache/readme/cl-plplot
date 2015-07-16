#[cl-plplot](http://common-lisp.net/project/cl-plplot/)#
The Common Lisp / CFFI based interface to the [PLplot](http://plplot.sourceforge.net/) Scientific Plotting Library.

##News##

####2015-05-06####
Finished updating the Lisp examples to match the PLplot examples for v5.11.

####2014-12-13####
Added gtk-example.lisp demonstrating how to combine cl-plplot and [cl-cffi-gtk](http://www.crategus.com/index.php/projekte/).

####2014-07-05####
Added commonqt-plot package demonstrating how to combine cl-plplot and [CommonQt](http://common-lisp.net/project/commonqt/).


##Layout##

####cl-plplot####
This is currently 18 files (in src/window).
* axis-label.lisp handles the axis-label and 3D-axis-label object.
* axis.lisp handles the axis object.
* bar-graph.lisp handles the bar-graph object.
* classes.lisp specifies all the cl-plplot classes.
* color-table.lisp handles plplot cmap0 color.
* contour-plot.lisp handles the contour-plot object.
* extended-color-table.lisp handles plplot cmap1 color.
* macros.lisp contains the important macros.
* package.lisp defines the package & the global variables.
* plot.lisp defines the plot object as well as providing an interface whereby the user can define their own custom plot objects.
* surface-plot.lisp handles the surface-plot object.
* text-item.lisp handles the text-item object.
* text-label.lisp handles the text-label and 3D-text-label object.
* utility-functions.lisp is a collection of low-level functions.
* window.lisp handles the window object.
* x-y-plot.lisp handles the x-y-plot object.
* 3D-mesh.lisp handles the 3D-mesh object.
* 3D-window.lisp handles the 3D-window object.
 
The file src/examples/window-examples.lisp contains examples of how to use 
cl-plplot to generate different 2D plots.


####cl-plplot-system####
This is currently 5 files (src/system).
* loadlib.lisp defines the package and loads the PLplot library.
* types.lisp defines the C types that are used in the PLplot library.
* pl-defcfun.lisp defines the macro that is used to facilitate wrapping almost all the CFFI calls.
* misc.lisp contains some helper functions, macros and struct definitions.
* api.lisp contains all the plplot library / CFFI interface calls. Not every function in the plplot API is available, though all the ones that are associated with drawing/graphing should be. Others can of course be added as desired.

The file system-examples.lisp contains some examples of how you might directly use the functions in cl-plplot-system. Also, src/examples/ contains Lisp versions of all the standard PLplot examples. Other useful resources are the documentation that comes with plplot, the examples that come with plplot, the (admittedly somewhat sparse) comments in api.lisp & possibly plplot.h.

A callback has been added that should trap PLplot when it tries to call exit() and instead cause a Lisp side error to be thrown. If you find a PLplot error that is not trapped by this callback please let me know.

