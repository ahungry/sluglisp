# cl-geocode: a geocoding API for Common Lisp

Geocoding is the process of finding associated geographic coordinates
(expressed as latitude and longitude) from other geographic
data, such as street addresses, or zip codes (postal codes).

Reverse geocoding is the opposite: finding an associated textual
location such as a street address, from geographic coordinates.

This API does both.

cl-geocode is loaded via ASDF and the file cl-geocode.asd.

## Examples

    cl-user(12): (asdf:load-system :cl-geocode)
    ...
    cl-user(13): (use-package :cl-geocode)
    t
    cl-user(14): (setq cl-geocode:*default-key* "...")
    "..."
    cl-user(15): (place-to-location "Berkeley, CA")
    #<location 37.871593,-122.27274>
    cl-user(16): (location-to-place *)
    "Berkeley, California"
    cl-user(17): (location-to-place
		    (place-to-location
		     "2629 College Ave, Berkeley, CA"))
    "Berkeley, California"
    cl-user(18): (place-to-location "2629 College Ave, Berkeley, CA")
    #<location 37.862926,-122.25329>
    cl-user(19): (distance-between
		    (place-to-location "Boulder, CO")
		    (place-to-location "Piedmont, CA"))
    923.7708985128291d0
    cl-user(20): (distance-between
		    (place-to-location "Lake Wylie, SC")
		    (place-to-location "Piedmont, CA"))
    2280.312157508608d0
    cl-user(21): 

Examples of the lower-level parts of the API:

    cl-user(4): (setq location1
		  (make-location :latitude 38.005 :longitude -121.804726))
    #<location 38.005,-121.804726>
    cl-user(5): (setq location2
		  (make-location :latitude 37.824444 :longitude -122.23055))
    #<location 37.824444,-122.23055>
    cl-user(6): (distance-between location1 location2)
    26.379927592149343d0
    cl-user(7): (distance-between location1 location2 :unit :kilometers)
    42.46016888326334d0
    cl-user(8): 

    ;; The following examples inspired by
    ;;   http://nationalatlas.gov/articles/mapping/a_latlong.html#four

    ;; A degree of latitude should be approximately 69 miles:
    cl-user(8): (distance-between
		 (make-location :latitude 38.0 :longitude -121.0)
		 (make-location :latitude 37.0 :longitude -121.0))
    69.16739825655358d0
    cl-user(9): 

    ;; A degree of longitude (at the equator) is also approximately 69
    ;; miles:
    cl-user(9): (distance-between
		 (make-location :latitude 0.0 :longitude -121.0)
		 (make-location :latitude 0.0 :longitude -122.0))
    69.16739825652837d0
    cl-user(10): 

    ;; But as you move north or south the size gradually decreases to zero as
    ;; the meridians converge at the poles:

    cl-user(10): (distance-between
		  (make-location :latitude 37.0 :longitude -121.0)
		  (make-location :latitude 37.0 :longitude -122.0))
    55.23928644738535d0
    cl-user(11): (distance-between
		  (make-location :latitude 50.0 :longitude -121.0)
		  (make-location :latitude 50.0 :longitude -122.0))
    44.459615443471485d0
    cl-user(12): (distance-between
		  (make-location :latitude 80.0 :longitude -121.0)
		  (make-location :latitude 80.0 :longitude -122.0))
    12.010644812832272d0
    cl-user(13): (distance-between
		  (make-location :latitude 90.0 :longitude -121.0)
		  (make-location :latitude 90.0 :longitude -122.0))
    0.0d0
    cl-user(14): 

Sometimes it is nice to see if a location is near another:

    cl-user(15): (location-near-p
		  (make-location :latitude 37.824444 :longitude -122.23055)
		  (make-location :latitude 38.005 :longitude -121.804726)
		  1.0)
    t
    cl-user(16): (location-near-p
		  (make-location :latitude 37.824444 :longitude -122.23055)
		  (make-location :latitude 38.005 :longitude -121.804726)
		  0.5)
    t
    cl-user(17): (location-near-p
		  (make-location :latitude 37.824444 :longitude -122.23055)
		  (make-location :latitude 38.005 :longitude -121.804726)
		  0.1)
    nil
    cl-user(18): 

## The complete API

### Types

A location is a structure containing a latitude and longitude.  The
accessors are:

    location-latitude
    location-longitude

and the constructor is

    make-location

### Variables

    *default-key*

The value of this is a string representing your Google Maps API
key. Obtain your own Google Maps API key from
[here](http://www.google.com/apis/maps/signup.html).

### Functions

Functions which operate on locations are:

    distance-between location1 location2 &key (unit :miles)

This calculates the straight line distance on the surface of the earth
between **LOCATION1** and **LOCATION2** using the "Great Circle
Distance Formula".  The values of the keyword **UNIT** can be :miles
(the default), :nautical-miles or :kilometers.

    location-near-p location reference within

Return true if **LOCATION** is near **REFERENCE** (a location) to
**WITHIN** decimal degrees in both latitude and longitude.

    place-to-location place &key (key *default-key*)

Return the location corresponding to **PLACE**.

    location-to-place location

Return the place corresponding to **LOCATION**.

## Implementation Details

**place-to-location** is pretty straightforward.  The Google Maps API
converts the place to a location.

**location-to-place**, however, is a little trickier.  There is no
readily available web resource that will do this (like Google Maps). I
did find The [Zip Code Database
Project](http://sourceforge.net/project/showfiles.php?group_id=111073)
which contains location coordinates for each zip code, and the data
includes the names of the cities for each zip code.  It's a fairly
simple matter to find the nearest match for a set of coordinates.

So, imbedded in cl-geocode is the function **location-to-zipcode**:

    cl-user(24): (location-to-zipcode (place-to-location "Oakland, CA"))
    #<zipcode 94612 (Oakland, California): 37.809425,-122.27172>
    cl-user(25): 

and

    cl-user(31): (location-to-zipcode (place-to-location "1 Infinite Loop"))
    #<zipcode 94087 (Sunnyvale, California): 37.35009,-122.03602>
    cl-user(32): 
