# Pounds
Pounds is intended to provide a collection of tools for operations on block storage. At present its only useful
features is a stream class over the top of mmaped files and (yet another) logging system which writes to a circular mmaped log.

## 1. File mappings
The base functionality is provided by mmap'ing files and defining a stream class to read/write them. 

### 1.1 Usage

```
;; open a 1MB file and mmap it
(defparameter *mymap* (open-mapping "mymap.dat" (* 1024 1024)))

;; make a stream from the mapping and write to it
(with-mapping-stream (s *mymap*)
  (write-sequence #(1 2 3 4) s))

(close-mapping *mymap*)
```

If the file does not exist it is created and extended to by SIZE bytes. If the file already 
exists it is simply opened with the original contents intact, the file will be extended if required.

### 1.2 Platform support
It works on Windows and Linux. I've not tried on other platforms (BSD, Darwin etc) but since they use the same 
POSIX APIs as Linux it should work without problem. 

## 2. Circular logs
The package POUNDS.LOG (nickname PLOG) implements a circular log by writing to the mmap'ed file. 

### 2.1 Motivation
Long running services need to record their activity. For instance, who mounted an NFS, what files they 
accessed etc. This is needed for two reasons: both for auditing service usage, and as a record of what 
the service actually did. We need this because when things go wrong, you need a record of what happened
to analyze. 

We therefore need a logging system which has the following properties:
* Can write an arbitrary number of messages without running out of space. This means a circular log.
* Writing to the log must have a very low-latency. Writes should never block for a significant time.
* We don't care as much about reading from the log because we will only ever read from the 
log after the events they describe have occured. 
* Stability and reliability are critical.
* It must be portable, working on Windows and Linux is mandatory.

We can use the file mapping stream to satisfy the above requirements.

### 2.2 Usage

Open a log file (creating it if it doesn't exist) by calling OPEN-LOG. If no pathname 
is provided, a file in the local directory named "pounds.log" will be created. 
The defualt size is 16k blocks of 128 bytes each (so a 2MB file). This means it can 
store a maximum of 16k messages. If a message is larger than the block size it will 
use multiple contiguous blocks.

Write a message to the log using WRITE-MESSAGE.

E.g.
```
(defvar *mylog* (pounds.log:open-log))

(pounds.log:write-message *mylog* :info (format nil "Hello from Lisp. 1 + 2 == ~A" (+ 1 2)))

(pounds.log:close-log *mylog*)
```

Follow the log's output by using the command:
```
(pounds.log:start-following *mylog*)

(pounds.log:stop-following)
``` 
This starts a thread which monitors the log and prints messages to the output stream 
as they are written to the log. 

You may dump the contents of the log to a stream, e.g.
```
(with-open-file (f "output.txt" :direction :output :if-exists :supersede)
  (pounds.log:dump-log *mylog* :stream f))
```

The log operators (READ-MESSAGE and WRITE-MESSAGE) support both multi-thread and multi-process access to the log files.

### 2.3 Sharing logs

It is often the case that different modules of the same program want to write to the same log, but 
with a tag to differentiate them. 
```
(defvar *the-log* (open-log))

(let ((tag (babel:string-to-octets "LOG1")))
  (defun write1 (lvl format-control &rest args)
    (write-message *the-log* lvl (apply #'format nil format-control args) 
                   :tag tag)))
(let ((tag (babel:string-to-octets "LOG2")))
  (defun write2 (lvl format-control &rest args)
    (write-message *the-log* lvl (apply #'format nil format-control args)
                   :tag tag)))
```

### 2.4 Performance

I've not done any rigorous benchmarking but performance feels sufficiently good. I tested on my 
crummy old laptop (1.5Ghz core 2 duo, 1GB RAM running Windows 8.1).


I wrote 100,000 messages to a standard 2MB (16k/128) log and it took 100 seconds, SBCL maxed out 1 of my cores. 
Taking 1ms per write seems acceptable. 
```
(time
  (dotimes (i 100000)
    (pounds.log:write-message :info *mylog* (format nil "Hello ~A" i))))
```

The equivalent experiment using LOG4CL took over 200 seconds to write 10,000 messages 
before I gave up waiting. Both SBCL and emacs were maxing out a core each. 
```
(time  
  (dotimes (i 100000)
    (log:debug "Hello ~A" i)))
```

## 3. Fixed block-size database
The package `POUNDS.DB` provides a simple fixed block-size database. See the example in example/db-test.lisp.
It can be used to provide shared access to a mmaped array of fixed-sized blocks, each of which stores a value.
You can lookup, add and delete entries.

## 4. Notes

The primary development platform was SBCL 1.2.7 on Windows 8.1. It has also been used on 32 and 64-bit Linux 
without any trouble. It may work on other unix-like platforms (e.g. BSD, Darwin) but I've not tested it there. 

## License

Released under the terms of the MIT license.

Frank James
March 2015.