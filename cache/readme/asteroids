asteroids
=========

Classic asteroids arcade game in Common Lisp, using SDL.

Originally ported from Python by andyhd (thanks, andyhd!).
The game now has sounds, keyboard 
controls, a built-in REPL (triggered by ESC key) and is very playable.




Installation (no git cloning needed)
------------------------------------

Install SBCL (or CCL) and quicklisp 
<a href="http://xach.livejournal.com/278047.html">as described here</a>)

<b>IMPORTANT</b> - install native sdl libraries!  For linux, that means apt-get install

- libsdl-dev
- libsdl-gfx1.2-dev
- libsdl-mixer1.2-dev

Now, in lisp 
(ql:quickload "asteroids")
(asteroids:main)

Quicklisp updates monthly.  If you want the newest version, git clone this repo
into an asdf-visible directory <a href="http://xach.livejournal.com/278047.html">(see Xach's article)</a>.
  ql:quickload will load the local repo first


Play Instructions
-----------------

The game plays like original asteroids, except instead of saucers you get 
powerups.  Ram the powerups to pick up super-missiles that go through anything,
shields, or to stop time.

Keyboard controls:

Q   - quit 
A   - rotate ship left
F   - rotate ship right
J   - thrust
SPC - fire
ESC - REPL

P starts the game in attract mode.

Notes
-----

Code has been re-organized and cleaned up.  Names have been altered
to make it more sensible and bugs fixed.  Now loading with quicklisp

ESC now brings you to (cla REPL.  Examine the game state, recompile anything
(carefully).

I also added audio using MAME asteroids samples.  lispbuilder-sdl-mixer is 
fragile and very sensitive to being (defun foo (x y)
    (declare (type integer x y))
    (logxor x y))shut down correctly in an interactive 
environment.

Background music is courtesy http://teknoaxe.com/Home.php.

TODO:
-----

* allow remapping of keys
* fix pause