cl-geoip
========

A simple cffi-wrapper around some basic functionality of libGeoIP as
provided by the debian package
http://packages.debian.org/wheezy/libgeoip-dev (original source
http://www.maxmind.com). I only implemented the functionality I
actually need, and released it since it might be useful for other
people. If anybody needs more functionality, patches are welcome!

I tested this under SBCL, Linux x64.

Copyright
---------

    Copyright (C) 2013 (let ((n "Christoph-Simon Senjak")) #| |#
     (format nil "~A <~C~C~C~C~A>" n (elt n 0) (elt n 10) (elt n 16)
     #\@ "uxul.de"))

    This program is free software. It comes without any warranty, to
    the extent permitted by applicable law. You can redistribute it
    and/or modify it under the terms of the Do What The Fuck You Want
    To Public License, Version 2, as published by Sam Hocevar. See
    http://www.wtfpl.net/ for more details.

Usage
-----

Under Debian/Ubuntu, the packages libgeoip-dev and geoip-database
should provide the necessary files to get started.

Before we can use the databases, they have to be initialized using
`open-databases`, which wants two file names, one for the IPv4 and one
fore the IPv6 database. Under Debian/Ubuntu, the following should be
correct:

    (geoip:open-databases "/usr/share/GeoIP/GeoIP.dat"
                          "/usr/share/GeoIP/GeoIPv6.dat")

After that, you should be able to find Country-Codes for IP-Strings
using `get-country-code`. If it is not found, NIL is returned:

      CL-USER> (geoip:get-country-code "188.40.170.99")
      "DE"
      CL-USER> (geoip:get-country-code "2a01:4f8:101:281:7ad5:1003:0:1")
      "DE"
      CL-USER> (geoip:get-country-code "some clusterfuck")
      NIL
