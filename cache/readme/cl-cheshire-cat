# Summary

cl-cheshire-cat is a project for a CL redirection server.

The core of the server is based on Edi Weitz (http://weitz.de/) CL librairies,
in particular:

 * Hunchentoot (Web Server)
 * CL-PPCRE

The persistence layer is ensured using CL-STORE
(http://common-lisp.net/project/cl-store).

# Installation

The recommended way to install cheshire is by using quicklisp as much as
possible:

 1. Using [`quicklisp`](http://www.quicklisp.org/), you can install:
  * `hunchentoot`
  * `cl-store`
  * `split-sequence`
 2. You need to manually install `sb-daemon`

If you use the provided `cheshire.sh` and `cheshire.lisp`, you also need to
install `py-configparser` (available via quicklisp).

# How to use

The recommended usage requires you to use SBCL (and optionnaly swank) and is
divided in a three component process:

 1. The `cheshire.sh` script which is responsible for daemon management
 operations.
 2. The `cheshire.lisp` script which is responsible for loading and
 starting the cheshire daemon.
 3. The daemon itself.

You are encouraged to modify and adapt the first two component in the way most
convinient to your own use of the Cheshire.

As mentionned, the default process is dependent on SBCL. However we would like
Cheshire to be compatible with as many CL distributions as possible. Any
adaptation of the starting process be compatible with a different CL
distribution is welcome.

## cheshire.sh

The daemon operations are performed using `scripts/cheshire.sh`. The
first argument of the script is either `start`, `stop`, `restart` or `status`
and makes the script perform the appropriate action. The script should always be
executed as root.

If you copy this script directly into `/etc/init.d/`, there is a slight
possibility that the script will run when started as root, but not at boot time
(e.g. on Debian 6.0). In this case, you will need to specify locale and/or home
in the configuration file. That's because the environment is not exactly the
same one when the service is started at boot time.

Second, if you see that the error log is populated with messages "Cannot open
such file" or "Cannot create such directory" when you use daemonize and
privilege dropping, make sure all the directories Cheshire tries to right into
are owned by the appropriate user. This include in particular `home/.cache`,
`home/.slime` and their sub-directories.

The second argument is expected to be a configuration file. If none is given,
`/etc/cheshire.conf` is the default configuration file.

cheshire.sh is looking for a `pidfile=` entry in the configuration
file. If there is no such entry, `/var/run/cheshire.pid` is used.

The required action is then performed by `cheshire.sh`. If it needs to run the
server, it assumes that `scripts/cheshire.lisp` is located as
`/usr/share/cheshire/scripts/cheshire.lisp` unless it finds a `system=`
directive in the configuration file.

## cheshire.lisp

`cheshire.lisp` is loading the configuration file and starting the
daemon. (Please refer to `config/cheshire.conf` for the details of the
documentation). `cheshire.lisp` expects the configuration file as its
first command-line argument and assumes that the cl-cheshire-cat system is
loadable via `(asdf:load-system "cl-cheshire-cat")`.

The starting process includes:

 1. starting to listen on the specified port
 2. daemon bookeeping (debugging, dropping privileges, swank loading)
 4. loading the redirection rules

The privileges are dropped after Cheshire started listening. This is a
limitation from usocket and hunchentoot. This means:

 * you should not have any problem listening directly on the 80 port (or any
   other privileged port).
 * the few first requests could happen to be served as root. Since the
   redirection specifications is not loaded untill the privileges are dropped
   and this issue can be prevented by a system firewall (like iptables), we
   don't consider it a big security issue.

## Customization 

If the recommended usage does not fit your use of Cheshire, feel free to adapt
any of the previously described steps. They are made to be highly and easily
customizable.

Note that the debugging bookeeping step in `cheshire.lisp` is provided for
convinience. If you skip this part, Hunchentoot defaults will be used:

 * the debugger will never be called in case of an error
 * error pages will show complete error message and backtrace

# Behavior

The server is listening on the HTTP port (80), awaiting an HTTP query. Other
ports will work, but you must keep in mind that no port rewritting facility is
currently offered.

The server first search for a domain name rule to apply and then for a URI
rewritting rule. The details of each rule is descript is the following section.

The rules are always checked _in turn_, if a rule matches, it's applied (and the
search stop).

If no domain name matching rule is found and the domain name does not start with
`www.`, the server will send a "301 Moved Permanently" to the same URL with an
additional `www.` prefix to domain name. If the domain name already starts with
`www.`, the server will return a "404 Not Found" error in order to avoid
redirection loop.

If a domain name rule matched, Cheshire then search its list of URI rule to
apply any addition URI modification. If there is no URI matching rule and no
domain rewritting in effect, the server will return a "404 Not Found" error in
order to prevent redirection loops.

Domain name rules parameters are used as default for its associated URI rules.

The loop protections intend to protect you from mild obvious rule specification
errors. Their goal is not to prevent willingful redirection loops or
mischievious configurations. For example it is easy to trick Cheshire into
issuing infinite redirection loops with a domain name rule with no URI rule, key
`(:exact "www.domain.example")` and replacement `"www.domain.example"`. In other
words you are responsible for the correctness of your redirection
rules. Cheshire will not check them for you and there is no plan to do so in the
future.

## Unspecified behavior

Since Cheshire intends to be a production-grade product, we made our best to
keep unspecified behavior implementation on a fail-early and safe basis.

For example, if you provide an invalid argument for a rule specification, we try
to fail and send an error message when you create or update the rule rather than
at apply time.

However, this behavior may not be always easy to have, don't forget that
unspecified behaviour is still unspecified and may make Cheshire to crash on
each and every request it receives.

# Rules

Each rule is composed of two main elements:

 * The rule's key is identifying whether the rule can be applied to a
   given URL. The key is itself composed of two elements:
  * The kind specifies the algorithm used to match the rule.
  * The match specification gives the information to match the rule against the
    URL.
 * The rule's effect including the modification to apply to the matching URL,
   options for the HTTP Answer or subsequent rules to apply.

## Domain rules

## Key

Three rules matching algorithms (kinds) are used:

 * `exact`
 * `suffix` (matches if the end of the domain name matches)
 * `regex`

Following the DNS specification
([[RFC1035](https://www.ietf.org/rfc/rfc1035.txt)]), domain name matches are
always case-insensitive.

## Effect

### Domain rewritting

If the rewrite specification is not nil, the domain will be rewritten.

If the rule is an `exact` match, the rewritte specification is use to replace the
whole domain name.

If the rule is a `suffix` match, the rewritte specification may contain once the
special substring ``"\1"``, which will be replaced by the prefix (non matching part)
of the original domain name.

If the rule is a `regex` match, any string replacement accepted by
`cl-ppcre:regex-replace` will be accepted and the result will be equivalent to
`(cl-ppcre:regex-replace regex original-domain-name replacement)`. Function
designators are not allowed since they cannot be saved easily.

## URI rules

A list of URI rewritting rules can be used for each domain.

### Key

Three rules matching algorithms (kinds) are used:

 * `exact`
 * `prefix` (matches if the beggining of the URI matches)
 * `regex`

URI match are case-sensitive by default (this can be changed via regex flags).

URI replacements for `exact` and `regex` match occurs the same way as Domain
rewritting.

If the rule is a `prefix` match, the rewritte specification may contain once the
special substring `"\1"`, which will be replaced by the suffix (non matching part)
of the original URI.

## Redirection parameters

Each redirection specification can also include redirection parameters.

### HTTP Status Code

The first parameter supported is `http-code` and its default value is 301.

`http-code` must be one of 300, 301 (default), 302, 303 or 307. This status code
will be the one used in the answer sent to the client. The behavior is
unspecified if an invalid status code specification is given.

### HTTP/HTTPS Protocol

The second parameter is `protocol` and its default value is `http`.

`protocol` must be one of `http` or `https` and it is the protocol that will be
used as the redirection target.

### Port

The third and last parameter is `port` and its default depends on the value of
`protocol` (443 if `protocol` is `https`, 80 otherwise).

`port` must be a valid port number (i.e. an integer between 1 and 63565).

## Query string manipulation

Each rule can be given a list of query string manipulation to do. These
operations are run before any rewritting occurs.

There are 5 operations which can be exectued on the list of get parameters:

The default behaviour is to leave the query string unmodified.

### Clear

The first operation is the basic clearing of the whole query string in order to
have a fresh start. Any operation executed before that one will be without
effect.

### Add

You can choose to add any get parameter to the URL. If the parameter already
exists, it will be twice in the query string, which may create unexpected
behaviour on the other side of the redirection.

The new parameter's value can be:

 * a constant string your giving to the operation
 * the path of the query (before rewritting)
 * the domain name of the query (before rewritting)

Parameters:

 * `name`: the name of the paramter you will create
 * `value`: the value of the parameter (a string, `:path` or `:domain`)

### Delete

Of course, if you can add, you can delete.

Parameter:

 * `name`: the name of the parameter to delete

### Update (the value)

The value update is performed by applying a regular expression substitution to
the value of the parameter.

Parameters:

 * `name`: the name of the parameter to update
 * `match`: the regular expression used to match the old value
 * `replacement`: the replacement string used to defini the new value

### Rename

Often, it's easier to rename a parameter than delete and re-create it.

Parameters:

 * `name`: the old name of the parameter
 * `new-name`: the new name of the parameter

# Management

Management options can be setup when creating the server.

Currently two management options are supported:

 * `admin-host`: The domain name used to manage the server (default is
 `"management.invalid"`). 
 * `admin-allowed`: A list of CIDR blocks. IP addresses in these blocks
 are allowed to manage the server (default is localhost only)

Each CIDR network specifications is a pair of two elements. The first one is an
IPv4 address (either a string in dotted notation `"127.0.0.1"` or a vector of
four integers in host order `#(127 0 0 1)`). The second is the prefix-length of
the CIDR block. If the second part is missing, its default value is 32.

The recommended tool to manage Cheshire is [curl](curl.haxx.se) or another low
level HTTP or TCP tool such as nc(1) or telnet(1).

The management API is splited in three parts:

 1. Global management
 2. Domain rules management
 3. URI rules management

Each operation is specified using three mecanisms:

 1. The URI of the operation (to choose what you want to do).
 2. The GET parameters (URL query string) to select the rule you want to manage.
 3. The POST parameters to provide the information required by the operation you want to perform.

## Global management

Global management operations are impacting the behavior of the whole server.

### Save the current rules

Path: `/save-rules`

POST Parameter:

 * `name` (optional): name of the file in which the rules will be stored. If the
parameter is not provided, the file used is the one from which the rules have
been pre-loaded (`rules-file`).

The file will be stored in the directory specified as the `rules-directory`
configuration and with the `crr` extension. If there is no such configuration,
the directory of the `rules-file` configuration will be used.

Example:

```
POST /save-rules HTTP/1.1
Host: management.invalid
Content-type: application/x-www-form-urlencoded

name=my-bkp
```

### Load a new set of rules

Path: `/load-rules`

POST Parameter:

 * `name` (optional): name of the file from which the rules will be loaded. If
   the parameter is not provided, the file used is the one from which the rules
   have been pre-loaded (`rules-file`).
 * `erase-all`: If this parameter is not given, an error message will be
   sent. Since loading a new set of rules delete any other rule currently in
   effect, this confirmation is here as a security.

The file will be loaded from the directory specified as the `rules-directory`
configuration and with the `crr` extension. If there is no such configuration,
the directory of the `rules-file` configuration will be used.

Example:

```
POST /load-rules HTTP/1.1
Host: management.invalid
Content-type: application/x-www-form-urlencoded

name=my-bkp
erase-all=OK
```

## Domain name rule management

The domain rule management operations are in the "folder"
`/domain-name-rule/`. For each of the following rule, the path will
therefore be prefixed by `/domain-name-rule`.

### List domain name rules

Path: `/list`

GET parameters (all optionals):

 * `kind`: The kind of the domain rule key (should be one of
   `exact`, `suffix` or `regex`.
 * `match`: A regular expression applied on the domain rule match
    specification.
 * `replacement`: A regular expression applied on the domain rule
    replacement specification.

Returns the list of domain names rules matching the parameters. If a criteria is
omitted all rules will match this criteria.

Example:

```
GET /domain-name-rule/list?kind=exact HTTP/1.1
Host: management.invalid
```

### Add a domain name rule

Path: `/add`

POST parameters:

 * `kind`: The kind of match string. Must be one of "exact", "suffix"
   or "regex".
 * `match`: The match string used to check whether the rule matches the
   current URL.
 * `replacement` (optional): The replacement string used to modify the URL part
   matching the rule.
 * `http-code` (optional): The HTTP Status code to be sent along for
   this redirection.
 * `position` (optional): Position at which the rule will be inserted
   in the rule list. If none is provided, the rule is inserted at the beginning
   of the list.
 * `protocol` (optional): Protocol to use after the redirection. Must be one of
   http or https.
 * `port` (optional): Port number to use after the redirection.

Example:

```
POST /domain-name-rule/add HTTP/1.1
Host: management.invalid
Content-type: application/x-www-form-urlencoded

kind=exact&match=www.domain.example&position=3
```

### Remove a domain name rule

Path: `/remove`

GET parameters:

 * `kind`: The kind of the rule.
 * `match`: The match spec of the rule.

POST parameter:

 * `confirmed`: This parameter must be non-nil.

Example:

```
POST /domain-name-rule/remove?kind=exact&match=www.domain.example HTTP/1.1
Host: management.invalid
Content-type: application/x-www-form-urlencoded

confirmed=OK
```

### Update a domain name rule

Path: `/update`

GET parameters:

 * `kind`: The kind of the rule.
 * `match`: The match spec of the rule.

POST parameters (all optional):

  * `new-kind`: The new kind to use for the rule from now on.
  * `new-match`: The new match spec to use for the rule from now on.
  * `new-replacement`: The new replacement to use for the rule from now
    on.
  * `new-http-code`: The new HTTP Status code to be used for this rule
    redirection from now on.

### Update the query string operations

The query string operations management is in the "folder"
`/query-string-updates/`. See Query string operation management for the details.

When applied to domain name rule, all these operations have two common GET
parameters:

 * `domain-name-kind`: The kind of the domain name rule.
 * `domain-name-match`: The match spec of the domain name rule.

## URI rule management

The URI rule management operations are in the "folder"
`/uri-rule/`. For each of the following rule, the path will
therefore be prefixed by `/uri-rule`.

All this operations need to know on wich parent domain name rule they operate.
Thus, each rule has two common GET parameters:

 * `domain-name-kind`: The kind of the domain name rule.
 * `domain-name-match`: The match spec of the domain name rule.

### List URI rules

Path: `/list`

GET parameters (all optionals):

 * `kind`: The kind of the domain rule key (should be one of
   `exact`, `prefix` or `regex`.
 * `match`: A regular expression applied on the domain rule match
    specification.
 * `replacement`: A regular expression applied on the domain rule
    replacement specification.

Returns the list of URI rules matching the parameters. If a criteria is omitted
all rules will match this criteria.

Example:

```
GET /uri-rule/list?domain-name-kind=exact&domain-name-match=www.domain.example&kind=exact HTTP/1.1
Host: management.invalid
```

### Add a URI rule

Path: `/add`

POST parameters:

 * `kind`: The kind of match string. Must be one of "exact", "suffix"
   or "regex".
 * `match`: The match string used to check whether the rule matches the
   current URL.
 * `replacement` (optional): The replacement string used to modify the URL part
   matching the rule.
 * `http-code` (optional): The HTTP Status code to be sent along for
   this redirection.
 * `position` (optional): Position at which the rule will be inserted
   in the rule list. If none is provided, the rule is inserted at the beginning
   of the list.
 * `protocol` (optional): Protocol to use after the redirection. Must be one of
   http or https.
 * `port` (optional): Port number to use after the redirection.

Example:

```
POST /uri-rule/add?domain-name-kind=exact&domain-name-match=www.domain.example HTTP/1.1
Host: management.invalid
Content-type: application/x-www-form-urlencoded

kind=prefix&match=%2Ffoo&position=2
```

### Remove a URI rule

Path: `/remove`

GET parameters:

 * `kind`: The kind of the rule.
 * `match`: The match spec of the rule.

POST parameter:

 * `confirmed`: This parameter must be non-nil.

Example:

```
POST /uri-rule/remove?domain-name-kind=exact&domain-name-match=www.domain.example&kind=exact&match=www.domain.example HTTP/1.1
Host: management.invalid
Content-type: application/x-www-form-urlencoded

confirmed=OK
```

### Update a URI rule

Path: `/update`

GET parameters:

 * `kind`: The kind of the rule.
 * `match`: The match spec of the rule.

POST parameters (all optional):

  * `new-kind`: The new kind to use for the rule from now on.
  * `new-match`: The new match spec to use for the rule from now on.
  * `new-replacement`: The new replacement to use for the rule from now
    on.
  * `new-http-code`: The new HTTP Status code to be used for this rule
    redirection from now on.

### Update the query string operations

The query string operations management is in the "folder"
`/query-string-updates/`. See Query string operation management for the details.

When applied to domain name rule, all these operations have two more common GET
parameters:

 * `uri-kind`: The kind of the domain name rule.
 * `uri-match`: The match spec of the domain name rule.

## Query string operations management

Query string management is currently very simple.

### List query string operations

Path: `/list`

GET parameters (all optionals):

 * `operation`: The operation filter of the domain rule key (should be one of
   `clear`, `add`, `delete`, `update` or `rename`).
 * Any combination of `name`, `new-name`, `match`, `replacement`: a regex on the
   corresponding field.

Returns the list of query string operations matching the parameters. If a
criteria is omitted all rules will match this criteria.

Example:

```
GET /domain-name-rule/query-string-updates/list?domain-name-kind=exact&domain-name-match=www.domain.example&operation=clear HTTP/1.1
Host: management.invalid
```

### Add a query string operation

Path: `/add`

POST parameters:

 * `operation`: The operation filter of the domain rule key (should be one of
   `clear`, `add`, `delete`, `update` or `rename`).
 * Any combination of `name`, `new-name`, `match`, `replacement` which is valid
   for this operation.
 * `position` (optional): Position at which the operation will be inserted in
   the operation list. If none is provided, the operation is inserted at the
   beginning of the list.

When you create an `add` operation, you should provide one of the following
parameters (they are mutually exclusive):

 * `value`: Its value is the value that will be given to the new GET parameter
 * `path-as-value`: If present, the path will be used as value for the new GET
   parameter
 * `domain-as-value`: If present, the domain name will be used as value for the
   new GET parameter

Example:

```
POST /uri-rule/query-string-updates/add?domain-name-kind=exact&domain-name-match=www.domain.example&uri-name=%2Ffoobar%2F&uri-kind=prefix HTTP/1.1
Host: management.invalid
Content-type: application/x-www-form-urlencoded

operation=add&name=redirect-from&domain-as-value=t
```

### Remove a query string operation

Path: `/remove`

GET parameters:

 * `operation`: The kind of the rule.
 * `name`: The name of the query parameter on which the operation is applied.
 * `match`: The match specification used in a query string update operation.

If `name` or `match` is not supported by the operation you want to delete, you
must omit them.

POST parameter:

 * `confirmed`: This parameter must be non-nil.

Example:

```
POST /uri-rule/query-string-updates/remove?domain-name-kind=exact&domain-name-match=www.domain.example&uri-name=%2Ffoobar%2F&uri-kind=prefix&operation=rename&name=domain-name-match HTTP/1.1
Host: management.invalid
Content-type: application/x-www-form-urlencoded

confirmed=OK
```

# Data Storage

The data are stored in memory by a hierarchy of classes, check `redirection-rule`
and subclasses.

# Limitations

 * The implementation restricted part should be extracted away in another
   package/project (TODO).

 * Because of the inherent multiple domain names property of the project,
  Cheshire supports only HTTP, no HTTPS now or in the forseenable future is
  planned.

 * No offline rule management.

# License

This software has been originally developed by Mathieu Lemoine
<mlemoine@mentel.com> and is the sole property of Mentel Inc. (www.mentel.com).
This software is distributed under the terms of the 3-clause BSD, stated as
follow:

Copyright © 2012, Mathieu Lemoine <mlemoine@mentel.com>, Mentel Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of "Mentel Inc." nor the names of its contributors may be
   used to endorse or promote products derived from this software without
   specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
