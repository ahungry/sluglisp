Ernestine
=========

[![Build Status](http://img.shields.io/travis/nlamirault/ernestine.svg)](https://travis-ci.org/nlamirault/ernestine)


Ernestine is a music management application.
see INSTALL file for more explanations.

2 frontends exists :
* a McClim player.
* a web radio in Flash.

## McCLim Player

### Dependencies

Install theses packages :

    $ sudo apt-get install mp3info aumix mplayer

### Launch the mcclim frontend

    CL-USER> (asdf:oos 'asdf:load-op :ernestine-gui)
    CL-USER> (ernestine-gui:player)

Add you music with the **C-i** command or Music button.
Put  in the line  the directory  which contains  you music  files. You
could use the File Browser (Browse button).
Then start the music scanning with the "Search" button.

### Available commands from the GUI

    C-q : quit ernestine

    C-Return : start playing selected song
    C-Space : stop playing current song

    C-n : next song
    C-p : previous song

    C-a : add to playlist selected song
    C-d : delete selected song from playlist
    C-z : shuffle

    C-i : import music files

    C-s : save current playlist
    C-o : open a file selector to search a playlist

### Screenshots

* [0.4](data/ernestine-mcclim-0.4.png)
* [0.3](data/ernestine-mcclim-0.3.png)
* [0.2](data/ernestine-clx-0.2.png)
* [0.1](data/ernestine-player-0.1.png)


## Web radio

### Launch the web frontend

    CL-USER> (asdf:oos 'asdf:load-op :ernestine-web)

### Creates the web radio

You must specify the virtual  host, the Ernestine source directory and
the port for the web server :

    CL-USER> (defparameter *radio*
                  (ernestine-web:create-web-radio "my.host.info" 9090
                                                  :covers-p t))

If *covers-p* is t, Ernestine will search covers, download and update
database of each album.

### Add music

If all  your music  is store in  directory /opt/music/, you  could add
this directory, and Ernestine will scan recursively all directories to
creates the music database :

    CL-USER> (ernestine-web:add-music *radio* "/opt/music/" t)

The *add-music* method take an optional argument : covers-p. If
it is t, Ernestine will download covers albums.

### Manage the web radio

    CL-USER> (ernestine-web:start-radio *radio*)
    CL-USER> (ernestine-web:stop-radio *radio*)

### Screenshots

* [0.4](data/ernestine-web-0.4.png)
* [0.3](data/ernestine-web-0.3.png)
* [0.3](data/ernestine-web-0.2.png)

## Benchs

Benchmarks of the database backend :

    CL-USER> (time
               (let ((backend (ernestine-database:get-backend :prevalence "/tmp/")))
                 (ernestine-tools:scan-directory backend "/opt/music/")))

### Version 0.2

    Date ; 2007-08-11
    Size : 16G for /opt/music/
    Without covers :
    Evaluation took:
      314.1 seconds of real time
      12.96081 seconds of user run time
      44.514786 seconds of system run time
      [Run times include 0.564 seconds GC run time.]
      0 calls to %EVAL
      0 page faults and
      230,012,408 bytes consed.

    With covers :
    Evaluation took:
      1211.078 seconds of real time
      370.02313 seconds of user run time
      147.08118 seconds of system run time
      [Run times include 188.276 seconds GC run time.]
      32040 calls to %EVAL
      15 page faults and
      93,196,758,304 bytes consed.

### Version 0.1

    Date : 2007-05-08
    Size : 14G for /opt/music
    Evaluation took:
      226.969 seconds of real time
      10.200638 seconds of user run time
      41.40659 seconds of system run time
      [Run times include 0.949 seconds GC run time.]
      0 calls to %EVAL
      0 page faults and
      263,101,968 bytes consed.


## Changelog

A changelog is available [here](ChangeLog.md).


## Contact

Nicolas Lamirault <nicolas.lamirault@gmail.com>
