Common Lisp port-based server manager
=====

Summary
-----

Manage port-based Common Lisp servers (e.g., Swank and Hunchentoot) through a unified interface.

Purpose
-----

Manage multiple instances of port-based servers through a unified interface. This is helpful if you want to quickly incorporate a console and a web server into your Common Lisp application. Swank is useful in providing an interactive interface to your application.

Quick setup (SBCL on Linux as example, YMMV)
-----

* Get Quicklisp. http://www.quicklisp.org/beta/

* Configure ASDF to be able to find it. Copy/paste (optionally adapt) the following shell commands:

```bash
mkdir -p $HOME/.config/common-lisp/source-registry.conf.d/50-hacking.conf
echo '(:tree (:home "hacking/common-lisp/"))' > $HOME/.config/common-lisp/source-registry.conf.d/50-hacking.conf
mkdir -p $HOME/hacking/common-lisp
cd $HOME/hacking/common-lisp
git clone https://github.com/pw4ever/cl-server-manager
```

* To quickly launch it, use the provided Shell script `$HOME/hacking/common-lisp/cl-server-manager/00-start-server.sh`. It loads the package (with the help of Quicklisp) and drops into prepl (portable REPL).

* An example session inside REPL.

```lisp
CL-USER> (ql:quickload :cl-server-manager) ; use Quicklisp to load CL-SERVER-MANAGER
To load "cl-server-manager":
  Load 1 ASDF system:
    cl-server-manager
; Loading "cl-server-manager"
........
(:CL-SERVER-MANAGER)
CL-USER> (in-package :cl-server-manager) ; save us from tying the package prefix
#<PACKAGE "CL-SERVER-MANAGER">
CL-SERVER-MANAGER> (launch-system-with-defaults nil :console :http) ; launch the console (Swank) and HTTP (Hunchentoot) servers 
(36325 #<HUNCHENTOOT:ACCEPTOR (host *, port 8085)>)
CL-SERVER-MANAGER> (list-servers) ; Which servers are available?
(:CONSOLE :HTTP)
CL-SERVER-MANAGER> (list-ports :name :console) ; which port the console server is using? now we can "slime-connect" to "127.0.0.1" with port "36325" to connect to this Lisp image
(36325)
CL-SERVER-MANAGER> (list-ports :name :http) ; which port the HTTP server is using? direct your browser to http://localhost:8085 
(8085)
CL-SERVER-MANAGER> (shutdown-server :name :http) ; shutdown the HTTP server
(NIL)
CL-SERVER-MANAGER> (shutdown-all-servers) ; quickly shutdown all servers
((T))
CL-SERVER-MANAGER> 
```

tips
-----

* SLIME's documentation facility (especially "slime-apropos-package" with keymap "C-c C-d p") helps understanding the user interface.

* Alternatively, take a look at the export list of "package.lisp."
