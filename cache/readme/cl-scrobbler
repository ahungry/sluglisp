# Cl-scrobbler

cl-scrobbler is a Common Lisp library for scrobbling to [last.fm](http://last.fm/).

It was initially designed to make it trivial to build a last.fm plugin for [shuffletron](http://github.com/redline6561/shuffletron/) but it should be useful to other hackers interested in scrobbling via CL, for whatever reason.

## Install
You could git clone this library and make sure it's path on your computer is on your ```asdf:*central-registry*``` but I _strongly recommend_ that you instead use Zach Beane's positively delightful [quicklisp](http://quicklisp.org/).

* Using quicklisp: Start your lisp. Run ```(ql:quickload :cl-scrobbler)```. Done!

## Getting Started
 * Install cl-scrobbler
 * Check out the [docs](http://redlinernotes.com/docs/cl-scrobbler.html) or see how I used it below.

## The Shuffletron Example
It is recommended to simply mirror the [shuffletron plugin](http://github.com/redline6561/shuffletron/blob/master/src/scrobbler.lisp). Here's a handwavy explanation of that code:

You should set ```*song-time-fn*``` and ```*song-info-fn*``` so that the
```*last-seek*``` and ```*song-info*``` variables can be updated with the
current track position and metadata, respectively. Then you can have hooks
in your application to call ```maybe-queue-scrobble``` when a song ends,
```update-song-info``` when a song begins, and, if applicable,
```update-last-seek``` after a seek finishes and ```update-skipped``` after
skipping the track.

Finally, you should fire off a thread to call ```scrobbler-init``` and
```scrobbler-loop``` which will do the work of creating or restoring
the session key and cache and then scrobbling tracks when possible.
