# CL-Sendmail portability project #

CL-Sendmail was written by Robert Marlow, as a rewrite of
the sendmail.lisp code by Christophe Rhodes.
Robert Marlow has not responded to my (and some other people's) patch
requests, which is why this is a fork of the copy of version 0.5.5
from
	<http://www.bobturf.org/software/cl-sendmail/>

We depend (through cl-mime) on cl-qprint. Unfortunately, version 0.2.0
(still in quicklisp as of November 2013) is broken, and we therefore
rely on the new maintained version of Max Rottenkolber available from
	<http://mr.gy/maintenance/cl-qprint/>
Note that the current git version there has a version number
of 1.0.0-rc that asdf does not like. To ensure you are not using the
broken cl-qprint I nevertheless added a versioned dependency in
cl-sendmail.asd, and you will either have to edit the cl-qprint.asd
file or cl-sendmail.asd, but at least it may force you to read this
README.

In this fork we strive for improved portability. In particular,
this version works with Allegro Common Lisp and needs no SBCL/CMUCL
internals anymore.

Please check the original README file too.

-- uuh
