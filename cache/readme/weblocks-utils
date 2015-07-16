# Weblocks-utils - utilities for Weblocks.

## About

Contains useful utilities, mostly to work with database (inspired by rails ActiveRecord methods).

## Usage and Documentation

Used as any package, I prefer to put it into :use part of parent packages. Documentation is generated for example by 

```
(atdoc:generate-html-documentation 
  '(:weblocks-utils) "doc/" 
     :single-page-p t 
     :index-title "Weblocks utils" 
     :heading "Weblocks utils")
```

after quickloading `weblocks-utils` and `atdoc` of course.

## Weblocks Assets Packages

For Weblocks plugins or extensions there is often need in external files like javascript code or stylesheets or something other.

Weblocks-utils contains `require-assets` function which downloads all needed stuff and serves downloaded files.

So we getting free from downloading assets and putting them into the right places with every new application created.

### Usage 

If our application/plugin/extension needs jQuery for normal work we need to put somewhere following code

```lisp
(weblocks-utils:require-assets "https://raw.github.com/html/weblocks-assets/master/jquery/1.8.2/")
```

and jQuery will be loaded into <getcwd>/assets/jquery-1.8.2/ and single file will be served from hunchentoot - /pub/scripts/jquery-1.8.2.js

We can just include script with url /pub/scripts/jquery-1.8.2.js somewhere.

Url we used here - https://raw.github.com/html/weblocks-assets/master/jquery/1.8.2/ is the url of *assets package*.

When `require-assets` is called inside some application it serves files with application prefix.

For example if our app has "/admin" prefix, code above will give us /admin/pub/scripts/jquery-1.8.2.js file.

`weblocks-utils:prepend-webapp-path` is useful for loading files, we can include jQuery by creating following dependency

```lisp
(make-instance 'script-dependency 
  :url (make-instance 'puri:uri 
    :path (weblocks-utils:prepend-webapp-path "/pub/scripts/jquery-1.8.2.js")))
```

We'll receive following result inside application with prefix "/admin"

```lisp
(weblocks-utils:prepend-webapp-path "/pub/scripts/jquery-1.8.2.js") ; => /admin/pub/scripts/jquery-1.8.2.js
```

### Assets Packages

*Assets package* is small piece of code which will 

* Download all necessary files on demand
* Serve all necessary files and directories

It contains 3 required files 

* version.txt - text file with single line. 

  Contains name of directory  which will be created inside of assets/. 

  Also it describes package name and version. 
  
  Inside of https://raw.github.com/html/weblocks-assets/master/jquery/1.8.2/version.txt 
  we have *jquery-1.8.2* and assets/jquery-1.8.2 directory will be created.

* get.sh - a bash script for downloading needed stuff. 

  It is executed from lisp during `require-assets` call and only when there is no package installed yet.

  You can go to assets/<package version> directory and evaluate bash get.sh if there will be problems with automatic installation.

* serve.lisp  - contains lisp code for serving files and directories. 

  Use function `serve-file` for serving files and `serve-directory` for directories.


Any url which contains these files can be used as *assets package* so anybody can create own *assets packages* and *assets packages repository*.

Here is example repository https://github.com/html/weblocks-assets 

It is used in 

* https://github.com/html/weblocks-twitter-bootstrap-application 
* https://github.com/html/weblocks-ajax-file-upload-presentation 
* https://github.com/html/weblocks-bootstrap-date-entry-presentation
* https://github.com/html/weblocks-cms

### Creating own Assets Packages and Assets Repositories

Any http server is suitable for serving *assets packages*. I'm using Github for these purposes since it allows to view repository files through http.

To create *assets package* you should create directory with files described above and to publish it.

#### For every version of software used you should create own *assets package*.

For example if I need to update some code for using jquery-seq 0.0.3 instead of 0.0.1, I'm creating *assets package* jquery-seq/0.0.3 in https://github.com/html/weblocks-assets 

It is just a copy of jquery-seq/0.0.1 directory with changed version.txt and get.sh files.

#### Every *assets package* should download specific version of software.

We can see code

```bash
wget https://raw.github.com/html/jquery-seq/8f1c86c8a21a35578760aa3efd6da3a318fec936/jquery-seq.js
```

in jquery-seq/0.0.1 and 

```bash
wget https://raw.github.com/html/jquery-seq/accfd715128a0251e768a2c8acb907fa5eeba42d/jquery-seq.js
```

in jquery-seq/0.0.3 downloads file of specific revision, 8f1c86c8a21a35578760aa3efd6da3a318fec936 in first case and accfd715128a0251e768a2c8acb907fa5eeba42d in second.

Using https://raw.github.com/html/jquery-seq/master/jquery-seq.js here would be mistake since it points to last version of file and code which uses 0.0.1 version would be broken with this link.

### Assets Packages Licenses

It is often required to have license file in software distributive.

Please remember that all assets files are downloaded automatically and you are responsive of accepting licenses of software inside of assets/*/ directories.

Please remember that created *assets package* should download license files if it is required.
