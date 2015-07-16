cl-crc64
========

cl-crc64 implements 64 bit cyclic redundancy checks in Common Lisp.

A cyclic redundancy check (CRC) is a type of hash function that
produces a checksum (a small, fixed number of bits) against a block of
data, such as a packet of network traffic or a block of a computer
file. The checksum is used to detect errors after transmission or
storage.

CRCs can also act as a kind of electronic fingerprint for identifying
blocks of data. SWISS-PROT + TREMBL use a 64-bit Cyclic Redundancy
Check for the amino acid sequences. My interest was in using CRC64 to
index data in a semantic web triple store.

This implementation is influenced heavily by [Python code by Gian
Paolo Ciceri](http://code.activestate.com/recipes/259177/) and
[crc.lisp by R. Matthew
Emerson](http://www.thoughtstuff.com/rme/crc.lisp).  My thanks to both
these authors.

For a more thorough discussion of CRC see W. H. Press, S.
A.Teukolsky, W. T. Vetterling, and B. P. Flannery, "Numerical recipes
in C", 2nd ed., Cambridge University Press. Pages 896ff.

The code comes with [BSD-style
license](http://www.opensource.org/licenses/bsd-license.php) so
you can basically do whatever you want with it.

Example
-------

cl-crc64 is ASDF installable, but assuming that you have
[QuickLisp](http://www.quicklisp.org/) installed:

	 Rob-Blackwells-MacBook:cl-crc64 reb$ sbcl
	 This is SBCL 1.0.29, an implementation of ANSI Common Lisp.
	 More information about SBCL is available at <http://www.sbcl.org/>.
	 
	 SBCL is free software, provided as is, with absolutely no warranty.
	 It is mostly in the public domain; some portions are provided under
	 BSD-style licenses.  See the CREDITS and COPYING files in the
	 distribution for more information.
	 * (ql:quickload "cl-crc64")
	 To load "cl-crc64":
	   Load 1 ASDF system:
	       cl-crc64
	       ; Loading "cl-crc64"
	       [package cl-crc64].
	       ("cl-crc64")
       * (use-package :cl-crc64)

       T
       * (initialise-crc64 +polynomial+)
       
       NIL
       * (format nil "~X" (crc64-sequence "IHATEMATH"))

       "E3DCADD69B01ADD1"
       * 


WARNING: This is old code that I've pulled out, repackaged and open
sourced. It was used as part of a demo some years back, but you
probably want to test it thoroughly for new projects.

Rob Blackwell

October 2010
