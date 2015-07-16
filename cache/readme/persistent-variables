persistent-variables
====================

1 Overview 
-----------

Persistent variables can be serialized to and from streams.  

They come with niceties, like restarts for failed serialization, late binding (loaded values are cached until the variable
is defined), and a test suite.

<pre class="src src-lisp">(defpvar *my-account-password*) <span style="color: #5f9ea0; font-style: italic;">;; </span><span style="color: #5f9ea0; font-style: italic;">Don't give it a value in the source file!</span>

<span style="color: #5f9ea0; font-style: italic;">;; </span><span style="color: #5f9ea0; font-style: italic;">At the Repl: </span>
(setf *my-account-password* <span style="color: #deb887;">"the-password"</span>)
(<span style="color: #00bfff; font-weight: bold;">with-open-file</span> (s <span style="color: #deb887;">"/path/to/somewhere/else"</span>)
  (p-save s))

<span style="color: #5f9ea0; font-style: italic;">;; </span><span style="color: #5f9ea0; font-style: italic;">Later, at startup:</span>
(<span style="color: #00bfff; font-weight: bold;">with-open-file</span> (s <span style="color: #deb887;">"/path/to/somewhere/else"</span>)
  (p-load s))
</pre>

</div>


1.1 Features 
-------------

   * Group variables into 'sets' that are saved and loaded independently.
   * Late binding of variables: load you the saved values before loading your code.
   * Ignores deleted/missing symbols and unbound symbols.  
   * Errors for serialization and deserialization problems, with nice restarts.
   * Low line of code count (around 100 lines of code)

2 Installation 
---------------

### 2.1 Quick Lisp 

Install <a href="http://www.quicklisp.org/beta/">Quick Lisp</a> and then run:

<pre class="src src-lisp">(ql:quickload 'persistent-variables)
</pre>

If you have problems, see the <a href="#26-getting-support">support</a> section, and you may want to <a href="#25-running-the-tests">run the tests</a>.

### 2.2 Gentoo 

As root, 

<pre class="src src-sh">emerge persistent-variables</pre>

Once the emerge is finished, the package can be loaded using ASDF:

<pre class="src src-lisp">(asdf:operate 'asdf:load-op <span style="color: #f08080;">:persistent-variables</span>)</pre>


If you have problems, see the <a href="#26-getting-support">support</a> section, otherwise you may want to <a href="#25-running-the-tests">run the tests</a>.

### 2.3 Ubunto 

<pre class="src src-sh">sudo apt-get install persistent-variables</pre>

Once the installation is finished, the package is loadable using ASDF:

<pre class="src src-lisp">(asdf:operate 'asdf:load-op <span style="color: #f08080;">:persistent-variables</span>)
</pre>

If you have problems, see the <a href="#26-getting-support">support</a> section, otherwise you may want to <a href="#25-running-the-tests">run the tests</a>.

### 2.4 Manual Installation 

In summary: Untar the <a href="https://github.com/WarrenWilkinson/persistent-variables/archive/master.tar.gz">.tar</a> package and then symlink the .asd files into a place where ASDF can find them. 


  1. Untar the files where you want them to be.  On windows download the <a href="https://github.com/WarrenWilkinson/persistent-variables/archive/master.zip">.zip</a> and unzip it instead, it's the same files.
  2. ASDF could be looking anywhere -- it depends on your setup.  Run this in your lisp repl to get a clue
     as to where ASDF is seeking libraries:

     <pre class="src src-lisp">(mapcan #'funcall asdf:*default-source-registries*)</pre>
  3. Symlink the .asd files to the source directory. If you use windows, <a href="http://bc.tech.coop/blog/041113.html">these     instructions on symlink alternatives apply to you.

Once the files are in place, the package can be loaded with ASDF by:

<pre class="src src-lisp">(asdf:operate 'asdf:load-op <span style="color: #f08080;">:persistent-variables</span>)</pre>

If you have problems, see the <a href="#26-getting-support">support</a> section.  If you don't have problems you may want to <a href="#25-running-the-tests">run the tests</a> anyway, because you can.

### 2.5 Running the Tests 

Once the system is loaded, it can be tested with asdf. 

<pre class="src src-lisp">(asdf:operate 'asdf:test-op <span style="color: #f08080;">:persistent-variables</span>)</pre>

This should display something like the following. There should
be <b>zero failures</b>, if you have failures see the <a href="#26-getting-support">support</a> section
of this document.

<pre class="src src-sh">RUNNING PERSISTENT-VARIABLE TESTS...
PERSISTENT-VARIABLE TEST RESULTS: 
     Tests: 8
   Success: 8
  Failures: 0
</pre>


### 2.6 Getting Support 

You can find support on this libraries <a href="http://warrenwilkinson.ca/persistent-variables">website</a> and/or <a href="https://github.com/WarrenWilkinson/persistent-variables">github</a> repository. Or you can email <a href="mailto:warrenwilkinson@gmail.com">Warren Wilkinson</a>.

3 License 
---------

Persister is distributed under the <a href="http://opensource.org/licenses/lgpl-2.1.php">LGPL2</a> License. 
