# burgled-batteries.syntax

This library provides Embedded Python syntax for burgled-batteries

To enable the syntax:

```lisp
(python.syntax:enable-python-syntax)
```

Python syntax is enclosed between brackets. The embedded syntax is very similar to Python's syntax, with some differences:

  * Identifiers staring with the `$` character are parsed as Lisp variable references.
  * To convert the Python expression result to a Lisp object, use the `^` operator.
  * Lisp expressions can be embedded in Python code, with this syntax: `$(<lisp expression>)`

Example:
```lisp
   (let (($url "http://pinterface.livejournal.com/data/atom"))
       [^feedparser.parse($url)])
```

Some examples:

```lisp
(import :feedparser)
[^feedparser.parse('http://pinterface.livejournal.com/data/atom')]
; => #<HASH-TABLE>
```

To explore what is happening behind the scenes, quote the Python expression:

```lisp
PYTHON> '[^.feedparser.parse('http://pinterface.livejournal.com/data/atom')]
(LET ((#:TRANSFORMED2520
       (CFFI:CONVERT-FROM-FOREIGN
        (CALL* (REF* "feedparser") "parse"
               (STRING.FROM-STRING*
                "http://pinterface.livejournal.com/data/atom"))
        'PYTHON.CFFI::OBJECT!)))
  #:TRANSFORMED2520)
```

Bigger example:
```lisp
PYTHON> (let (($cal [icalendar.Calendar()]))
  [$cal.add('prodid', '-//My calendar product//mxm.dk//')]
  (let (($event [icalendar.Event()]))
    [$event.add('summary', 'Python meeting about calendaring')]
    [$event.add('dtstart', datetime.datetime(2005,4,4,8,0,0))]
    [$event.add('dtend', datetime.datetime(2005,4,4,10,0,0))]
    [$event.add('dtstamp', datetime.datetime(2005,4,4,0,10,0))]
    (let (($organizer [icalendar.vCalAddress('MAILTO: noone@example.com')]))
      [$organizer.params['cn'] = icalendar.vText('Max Rasmussen')]
      [$organizer.params['role'] = icalendar.vText('CHAIR')]
      [$event['organizer'] = $organizer]
      [$event['location'] = icalendar.vText('Odense, Denmark')]

      [$event['uid'] = '20050115T101010/27346262376@mxm.dk']
      [$event.add('priority', 5)]

      (let (($attendee [icalendar.vCalAddress('MAILTO:maxm@example.com')]))
         [$attendee.params['cn'] = icalendar.vText('Max Rasmussen')]
         [$attendee.params['ROLE'] = icalendar.vText('REQ-PARTICIPANT')]
         [$event.add('attendee', $attendee, encode=0)])

      (let (($attendee [icalendar.vCalAddress('MAILTO:the-dude@example.com')]))
         [$attendee.params['cn'] = icalendar.vText('The Dude')]
         [$attendee.params['ROLE'] = icalendar.vText('REQ-PARTICIPANT')]
         [$event.add('attendee', $attendee, encode=0)])

      [$cal.add_component($event)]
      [^$cal.to_ical()])))
=>

"BEGIN:VCALENDAR
PRODID:-//My calendar product//mxm.dk//
BEGIN:VEVENT
SUMMARY:Python meeting about calendaring
DTSTART;VALUE=DATE-TIME:20050404T080000
DTEND;VALUE=DATE-TIME:20050404T100000
DTSTAMP;VALUE=DATE-TIME:20050404T001000Z
UID:20050115T101010/27346262376@mxm.dk
LOCATION:Odense\\, Denmark
MAILTO:MAXM@EXAMPLE.COM:attendee
MAILTO:THE-DUDE@EXAMPLE.COM:attendee
ORGANIZER;CN=\"Max Rasmussen\";ROLE=CHAIR:MAILTO: noone@example.com
PRIORITY:5
END:VEVENT
END:VCALENDAR"
```

For more examples, checkout the syntax tests or [this blog post](http://mmontone-programming.blogspot.com.ar/2014/09/embedding-python-in-common-lisp.html).
