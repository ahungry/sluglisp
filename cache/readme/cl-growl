# CL-Growl Client Library

[Growl][growl] is a notification system originally written for Mac OS
X, but now supported on [Windows][gfw] and [Linux][gfl].  Growl
supports a small network protocol over UDP called [GNTP][gntp].  This
is a Common Lisp client library for registering an application with
Growl and sending Growl notifications on behalf of the registered
application.

  [growl]: http://growl.info
  [gfw]: http://www.growlforwindows.com/gfw/
  [gfl]: http://mattn.github.io/growl-for-linux/
  [gntp]: http://growl.info/documentation/developer/gntp.php

## Obtaining the code

* Quicklisp: `(ql:quickload :cl-growl)`
* Git repository: <http://github.com/nklein/cl-growl/>

## Usage

Before your application can start sending messages to Growl, it needs
to register the notification types it intends to send. On the
receiving end, the message display can be customized based on the
notification type.  Here is a simple example showing some of the many
options to the `register` function for an application called _Lambda
Fun_ with four types of messages: _debug_, _info_, _warn_, and _error_
where _debug_ messages are not displayed unless you tweak your Growl
preferences to display them:

    (growl:register
       :app "Lambda Fun"
       :app-icon "http://nklein.com/favicon.ico"
       :enabled '( "info" "warn" "error" )
       :disabled '( "debug" )
       :host "localhost"
       :port 23053
       :password "growl-password"
       :checksum-mode :sha256)

See `(documentation #'growl:register t)` for a description of the
other parameters available when calling `register`.

After an application has been registered once (from anywhere, in any
process), you can send a message _Cannot connect to database!_ with
the title _Critical Error_ of type _Error_ with priority 2 that will
stay showing on the receiving desktop with:

    (growl:notify "Cannot cannoect to database!"
	              :title "Critical Error"
                  :priority 2
                  :sticky t)

See `(documentation #'growl:notify t)` for a description of the
other parameters available when calling `notify`.

Almost all of the parameters to both `register` and `notify` take
their default values from special variables which can be rebound by
your application.  Here are the available variables.  You can use
`(documentation <variable-name> 'variable)` to see documentation for
each of these variables.

    *growl-default-host*     ; initially "localhost"
    *growl-default-port*     ; initially 23053
    *growl-default-app*
    *growl-default-app-icon*
    *growl-default-salt*
    *growl-default-iv*


    *growl-default-notification*
    *growl-default-title*
    *growl-default-priority*
    *growl-default-icon*

    *growl-default-callback-context*
    *growl-default-callback-context-type*
    *growl-default-callback-target*

    *growl-default-origin-fields*
    *growl-default-custom-fields*
    *growl-default-application-fields*

    *growl-default-checksum-mode*
    *growl-default-encryption-mode*
    *growl-default-password*

## Compatibility

This library does not currently implement the `subscribe` protocol
method of GNTP.

Currently, the Mac OS X Growl server does not support any of the
encryption modes that Growl for Windows supports.
