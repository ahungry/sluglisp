# sqnc

sqnc is a music editor for hackers.  It's written in Common Lisp,
based on Emacs, makes sound with Csound and has a terminal interface
built on ncurses.

sqnc is my solution to many years of frustration with existing music
composition software.  It gets rid of the mouse, GUI windows, modal
dialogs, nested menus, MIDI, pretty instrument and effect plugins and
many other concepts taken for granted in modern sequencers and DAWs,
in favor of the keyboard, emacs-style buffers and tiling windows,
direct communication with synthesis software, and an extensible Common
Lisp environment.

## Disclaimer

sqnc is still very much a work in progress.  It will change suddenly,
and without warning.  It will break project files.  It will go without
updates for long periods of time, punctuated by periods of rapid
development.  Documentation is sparse or nonexistent, as things are
changing rapidly. I've put it on Github mainly to let others take an
early look at it.

sqnc is written bottom-up in a number of layers.  It's written in a
heavily modified Common Lisp.  I did this because I wanted to be able
to implement ideas very quickly and keep sqnc's codebase as small as
possible.  Unfortunately, the result is fairly inscrutable to anyone
unacquainted with the macros and utility functions in use.  You can
refer to the file \`util.lisp' for implementation details.  I'll be
converting it back into idiomatic Common Lisp in the near future to
make it easier for others to understand and contribute.

## Installation

sqnc has only been tested on sbcl running in the rxvt-unicode terminal
emulator. On the Lisp side, sqnc depends on cl-ncurses, cl-store, cffi
and (currently) swank.  You'll also need to have Csound installed,
including the Csound API.

It seems some of the dependencies for the Lisp packages above have
disappeared from their asdf-install locations, so you're on your own
either tracking down the dependencies yourself or using something like
clbuild.

The file \`sqnc.asd' will need to be somewhere in the
\`asdf:\*central-registry\*'. The easiest way to accomplish this is by
symlinking \`sqnc.asd' into \`~/.sbcl/systems/' like this:

    ln -s /path/to/sqnc/sqnc.asd ~/.sbcl/systems/

After that, just run the shell script \`run-sqnc.sh' in rxvt-unicode to
bring up sqnc. You can symlink \`run-sqnc.sh' into your $PATH to make
this quicker.

## Usage

sqnc is very similar to emacs. Many of its keybindings are identical.
Look at the file \`keys.lisp' for a list of all the default keybindings
and their commands.

## Configuration

Configuration and customization code should be put in a \`~/.sqnc' file
in your home directory. See the example \`.sqnc' file in the sqnc root
directory.

## The Future

sqnc is very much a work in progress.  It's my lifelong itch to
scratch, and I intend to make it the most powerful music editor in
existence.  The TODO list includes a native Cocoa rewrite to free sqnc
from the restrictions of the terminal, as well as to make installation
a simple drag-and-drop. Supercollider, VST and MIDI support are also
possibilities.

## License

Copyright (C) 2010 tlh

sqnc is distributed under The MIT License. See the file COPYING.
