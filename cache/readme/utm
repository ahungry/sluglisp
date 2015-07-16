This library converts coordinates in latitude/longitude to UTM and from UTM to latitude/longitude.

Here's a sample run showing all off essentially all of the functionality.

```commonlisp
    * (ql:quickload 'utm)
    To load "utm":
      Load 1 ASDF system:
        utm
    ; Loading "utm"
    
    (UTM)
    * (utm:lat-lon-to-utm 39.264657358 -105.396267073)
    
    465814.36361674307d0
    4346221.50650324d0
    13
    * (utm:utm-to-lat-lon 465814.36361674307d0 4346221.50650324d0 13)
    
    39.264654456410966d0
    -105.39627074290249d0
    * (utm:lat-lon-to-utm 39.264657358 -105.396267073 :ellipsoid "WGS72")
    
    465814.3743203891d0
    4346220.137838842d0
    13
    * (utm:utm-to-lat-lon 465814.3743203891d0 4346220.137838842d0 13 :ellipsoid "WGS72")
    
    39.26465442463012d0
    -105.39627074272353d0
    * (utm:ellipsoid-names)
    
    ("NAD83" "WGS84" "GRS80" "WGS72" "Australian1965" "Krasovsky1940"
     "International1924" "Hayford1909" "Clake1880" "Clarke1866" "Airy1830"
     "Bessel1841" "Everest1830")
    *
```

It is based heavily on information from Steve Dutch's [UTM info website](http://www.uwgb.edu/dutchs/FieldMethods/UTMSystem.htm) and [UTM formula website](http://www.uwgb.edu/dutchs/UsefulData/UTMFormulas.htm).

It does not make any attempt to correct for the polar regions, so the results for those areas may be incorrect.  To learn why UTM breaks at the polar regions, [see here](http://www.uwgb.edu/dutchs/FieldMethods/UTMSystem.htm).

The calculations seem to be fairly accurate.  Converting from lat/lon-> utm -> lat/lon returns the same latitude and longitude within 6 or 7 decimal places, which is probably more accurate than coordinates coming from a regular GPS unit.  The results also match up with several online converters, such as [this very good one](http://www.rcn.montana.edu/Resources/Converter.aspx) on the University of Montana's website.

