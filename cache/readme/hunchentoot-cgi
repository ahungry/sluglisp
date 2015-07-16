
# hunchentoot-cgi

A library for executing CGI scripts from the hunchentoot webserver
written by Cyrus Harmon.

To activate hunchentoot-cgi, create a new dispatcher-and-handler and
add this to the hunchentoot global dispatch table roughly as follows:

    (pushnew (hunchentoot-cgi:create-cgi-dispatcher-and-handler
              "/cgi-bin/"
              "/usr/local/share/hunchentoot/cgi/"
             hunchentoot:*dispatch-table*
             :test #'equal)

This will instruct hunchentoot to field requests to /cgi-bin/foo
and execute /usr/local/share/hunchentoot/cgi/foo as a CGI script.

