# Earthquake data via the USGS website with Common Lisp

Recently we had a bunch of small earthquakes in the Oakland, CA area.
I thought it would be fun to use Common Lisp to retrieve and
display information about recent earthquakes.

I found that the USGS website has hourly, daily and weekly quake data
available
[here](http://earthquake.usgs.gov/eqcenter/recenteqsww/catalogs/).

Possible formats of interest are XML (RSS) and CSV.  I decided to
use the CSV format, comma separated files, since XML seemed like
overkill.

What I wanted to do was to find recent earthquakes near where I lived.
However, the data from USGS is of the form:

    Src,Eqid,Version,Datetime,Lat,Lon,Magnitude,Depth,NST
    ak,00074326,5,"January 09, 2007 23:59:55 GMT",64.1478,-150.1976,1.8,1.00,07

Time, location (latitude and longitude in decimal degrees) and
magnitude, among other bits of data.  The data is nice, but I needed
two things to make it useful:

  1. filter out the coordinates I didn't want, and
  2. convert latitude/longitude coordinates into city names.

I wrote [cl-geocode](https://github.com/e40/cl-geocode) to do the
heavy lifting, which allows us to do something like this:

    cl-user(3): (get-quake-info (place-to-location "Oakland, CA")
      :period :week
      :larger-than nil
      :within 1.0)
    ;; Downloading quake data...done.
    (("Friday, November 12, 2010 20:02:05 UTC" "Gilroy, California"
      #<location 36.9678,-121.5598> 1.2)
     ("Friday, November 12, 2010 10:16:24 UTC" "Healdsburg, California"
      #<location 38.6048,-122.9313> 1.3)
     ("Friday, November 12, 2010 09:40:48 UTC" "San Francisco, California"
      #<location 37.7302,-122.566> 1.3)
     ("Friday, November 12, 2010 08:11:09 UTC" "Cobb, California"
      #<location 38.7955,-122.7797> 1.1)
     ("Friday, November 12, 2010 04:02:18 UTC" "California"
      #<location 37.4698,-121.8323> 1.9)
     ("Thursday, November 11, 2010 23:01:05 UTC" "Cobb, California"
      #<location 38.7518,-122.7188> 1.0)
     ("Thursday, November 11, 2010 21:23:16 UTC" "Cobb, California"
      #<location 38.802,-122.7972> 1.1)
     ("Thursday, November 11, 2010 20:52:56 UTC" "Spanish Flat, California"
      #<location 38.3727,-122.1635> 1.9)
     ("Thursday, November 11, 2010 16:00:51 UTC" "Cobb, California"
      #<location 38.788,-122.777> 1.6)
     ("Thursday, November 11, 2010 15:42:57 UTC" "Cobb, California"
      #<location 38.8022,-122.8117> 2.0)
     ...)
    cl-user(4): 

The :within keyword value is in decimal degrees.  A degree of latitude
is approximately 69 miles.  So 3.0 degrees is a little more than 200
miles in latitude.  I use this for longitude, as well, but a degree of
longitude changes with latitude, from 69 at the equator to 0 at the
north and south poles.

The :period keyword value can be :hour, :day or :week.

The :larger-than keyword is a filter on magnitude.  *nil* means all
quakes.  2.0 means those larger than magnitude 2.0.

## Installation

You need to download and install cl-geocode and cl-quakeinfo and to
register, with ASDF, the *.asd* files.  Then, you just need to:

    cl-user(1): (asdf:load-system :usgs)
    ; loading system definition from /home/layer/src/usgs/usgs.asd into
    ; #<The asdf0 package>
    ; Loading /home/layer/src/usgs/usgs.asd
    ; registering #<system usgs> as usgs
    ; loading system definition from
    ; /home/layer/src/cl-geocode/cl-geocode.asd into #<The asdf0 package>
    ; Loading /home/layer/src/cl-geocode/cl-geocode.asd
    ; registering #<system cl-geocode> as cl-geocode
    ; Fast loading /home/layer/src/cl-geocode/package.fasl
    ;   Fast loading /usr/local/acl82.64/code/aserve.004
    ;;; Installing aserve patch, version 3.
    ;     Fast loading /usr/local/acl82.64/code/acldns.001
    ;;; Installing acldns patch, version 1.
    ;   Autoloading for package "top-level.debug":
    ;     Fast loading from bundle code/autozoom.fasl.
    ; Fast loading /home/layer/src/cl-geocode/zip-util.fasl
    ; Fast loading /home/layer/src/cl-geocode/geocode.fasl
    ; Fast loading /home/layer/src/usgs/usgs.fasl
    ;; Try this:

    (get-quake-info (place-to-location "Oakland, CA")
      :period :week
      :larger-than nil
      :within 1.0)
    t

Don't forget to set your Google Maps API key:

    cl-user(2): (setq cl-geocode:*default-key* "...")

where what is in the quotes is the API key you obtained from Google.
See the instructions in cl-geocode for more information.
