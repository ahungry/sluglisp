<h1>glyphs</h1>

<p>A little experiment in reducing verbosity in Common Lisp, inspired by
BODOL (https://github.com/bodil/BODOL - no affiliation).</p>

<p>To try it out in your REPL you can use (ql:quickload :glyphs)
if you have added to your ASDF load path in local projects.</p>

<p>Update: Now that it&apos;s in quicklisp, simply:
<code>lisp
(ql:quickload :glyphs)
</code></p>

<h2>Examples</h2>

<h3>Factorial example with glyphs function macro:</h3>

<p>glyphs:
<code>lisp
(ƒ factorial
   0 → 1
   α → (* α (factorial (1- α))))
</code>
vs.:
<code>lisp
(defun factorial (x)
  (cond ((equal x 0) 1)
        (x (* x (factorial (1- x))))))
</code>
result:
<code>lisp
(factorial 8)
40320
</code></p>

<h3>Type safety with factorial</h3>

<pre><code><span class="code"><span class="paren1">(<span class="code">ƒ factorial
  0 → 1
  <span class="paren2">(<span class="code">and <span class="paren3">(<span class="code">numberp α</span>)</span> <span class="paren3">(<span class="code">&gt; α 0</span>)</span></span>)</span> → <span class="paren2">(<span class="code">* α <span class="paren3">(<span class="code">factorial <span class="paren4">(<span class="code">1- α</span>)</span></span>)</span></span>)</span>
  α → <span class="paren2">(<span class="code">error <span class="string">"Non-numbers (or negatives) don't work real well with factorials!"</span></span>)</span></span>)</span></span></code></pre>

<p>vs.:
<code>lisp
(defun factorial (x)
  (cond ((equal x 0) 1)
        ((and (numberp x) (&gt; x 0)) (* x (factorial (1- x))))
        (t (error &quot;Non-numbers (or negatives) don't work real well with factorials!&quot;))))
</code></p>

<h3>Reverse a list</h3>

<p>glyphs:
<code>lisp
(ƒ reverse*
  (cdr α) → (append (reverse* (cdr α)) `(,(car α)))
  α → α)
</code></p>

<h3>Map with glyphs lambda macro to compare strings and do a side effect</h3>

<p>glyphs:</p>

<pre><code><span class="code"><span class="paren1">(<span class="code">mapcar <span class="paren2">(<span class="code">λ <span class="string">"cat"</span> → <span class="paren3">(<span class="code">print <span class="string">"Cats rock"</span></span>)</span>
           <span class="string">"dog"</span> → <span class="paren3">(<span class="code">print <span class="string">"Dogs do too!"</span></span>)</span></span>)</span> '<span class="paren2">(<span class="code"><span class="string">"cat"</span> <span class="string">"dog"</span> <span class="string">"mouse"</span></span>)</span></span>)</span></span></code></pre>

<p>vs.:
<code>lisp
(mapcar (lambda (x)
          (cond ((equal x &quot;cat&quot;) (print &quot;Cats rock&quot;))
                ((equal x &quot;dog&quot;) (print &quot;Dogs do too!&quot;)) '(&quot;cat&quot; &quot;dog&quot; &quot;mouse&quot;))))
</code>
result:
<code>lisp
&quot;Cats rock&quot;
&quot;Dogs do too!&quot;
NIL
</code></p>

<h2>Comparison based on passed in conditionals</h2>

<p>glyphs:
<code>lisp
(ƒ double-odds-half-evens
  (oddp α) → (* x 2)
  (evenp α) → (/ x 2))
</code>
vs.:
<code>lisp
(defun double-odds-half-evens (x)
  (cond ((oddp x) (* x 2))
        ((evenp x) (/ x 2))))
</code>
result:
<code>lisp
(double-odds-half-evens 4)
2
(double-odds-half-evens 3)
6
</code></p>

<h3>Using multiple values by including special variables in the statement</h3>

<p>You can use multiple values by prefixing a variable in the statement
portion of the code with either a &lsquo;α&rsquo; or an &apos;?&apos;.</p>

<p>glyphs:
<code>lisp
(mapcar (λ α → (+ α αb αc)) '(1 2 3) '(4 5 6) '(7 8 9))
</code>
vs.:
<code>lisp
(mapcar (lambda (a b c) (+ a b c)) '(1 2 3) '(4 5 6) '(7 8 9))
</code></p>

<p>result:
<code>lisp
(12 15 18)
</code></p>

<h2>Fast matching based on regex strings</h2>

<p>Make sure to use the readtable that comes with it first:
<code>lisp
(in-readtable glyphs:syntax)
</code>
Or these little readtable shortcuts will not work.</p>

<p>glyphs:
<code>lisp
(ƒ any-cats?
  ~&quot;cat&quot;~ → (print &quot;yes!&quot;))
</code>
vs.:
<code>lisp
(defun any-cats? (x)
  (when (cl-ppcre:scan &quot;cat&quot; x)
    (print &quot;yes!&quot;)))
</code>
result:
<code>lisp
(any-cats? &quot;I see some cats&quot;)
&quot;yes!&quot;
</code></p>

<h2>Easy regex replaces on matching strings</h2>

<p>glyphs:
<code>lisp
(ƒ no-cats
  ~&quot;(were|cat)&quot;~ → |&quot;dog&quot;|)
</code>
vs.:
<code>lisp
(defun no-cats (x)
  (let ((regex &quot;(were|cat)&quot;))
       (when (cl-ppcre:scan regex x)
         (cl-ppcre:regex-replace-all regex x &quot;dog&quot;))))
</code>
result:
<code>lisp
(no-cats &quot;there were dogs and there were cats&quot;)
&quot;there dog dogs and there dog dogs&quot;
</code></p>

<h1>Currently used glyphs and bindings for them (more to come)</h1>

<h2>Emacs bindings</h2>

<p>Add to `.emacs&apos;</p>

<pre><code><span class="code"><span class="comment">;; Keybindings for glyphs
</span><span class="paren1">(<span class="code">global-set-key <span class="paren2">(<span class="code">kbd <span class="string">"M-l"</span></span>)</span> <span class="paren2">(<span class="code"><i><span class="symbol">lambda</span></i> <span class="paren3">(<span class="code"></span>)</span> <span class="paren3">(<span class="code">interactive</span>)</span> <span class="paren3">(<span class="code">insert <span class="string">"</span><span class="string">\u</span><span class="string">03bb"</span></span>)</span></span>)</span></span>)</span> <span class="comment">; λ lambda
</span><span class="paren1">(<span class="code">global-set-key <span class="paren2">(<span class="code">kbd <span class="string">"M-f"</span></span>)</span> <span class="paren2">(<span class="code"><i><span class="symbol">lambda</span></i> <span class="paren3">(<span class="code"></span>)</span> <span class="paren3">(<span class="code">interactive</span>)</span> <span class="paren3">(<span class="code">insert <span class="string">"</span><span class="string">\u</span><span class="string">0192"</span></span>)</span></span>)</span></span>)</span> <span class="comment">; ƒ function
</span><span class="paren1">(<span class="code">global-set-key <span class="paren2">(<span class="code">kbd <span class="string">"M--"</span></span>)</span> <span class="paren2">(<span class="code"><i><span class="symbol">lambda</span></i> <span class="paren3">(<span class="code"></span>)</span> <span class="paren3">(<span class="code">interactive</span>)</span> <span class="paren3">(<span class="code">insert <span class="string">"</span><span class="string">\u</span><span class="string">2192"</span></span>)</span></span>)</span></span>)</span> <span class="comment">; → right arrow
</span><span class="paren1">(<span class="code">global-set-key <span class="paren2">(<span class="code">kbd <span class="string">"M-a"</span></span>)</span> <span class="paren2">(<span class="code"><i><span class="symbol">lambda</span></i> <span class="paren3">(<span class="code"></span>)</span> <span class="paren3">(<span class="code">interactive</span>)</span> <span class="paren3">(<span class="code">insert <span class="string">"</span><span class="string">\u</span><span class="string">03b1"</span></span>)</span></span>)</span></span>)</span> <span class="comment">; α alpha
</span><span class="paren1">(<span class="code">global-set-key <span class="paren2">(<span class="code">kbd <span class="string">"M-y"</span></span>)</span> <span class="paren2">(<span class="code"><i><span class="symbol">lambda</span></i> <span class="paren3">(<span class="code"></span>)</span> <span class="paren3">(<span class="code">interactive</span>)</span> <span class="paren3">(<span class="code">insert <span class="string">"</span><span class="string">\u</span><span class="string">03c8"</span></span>)</span></span>)</span></span>)</span> <span class="comment">; ψ psi</span></span></code></pre>

<h2>Vim bindings</h2>

<p>Add to `.vimrc&apos;</p>

<pre><code>" Keybindings for glyphs
:inoremap &lt;A-l&gt; &lt;C-v&gt;u3bb&lt;Space&gt;   ; λ lambda
:inoremap &lt;A-f&gt; &lt;C-v&gt;u192&lt;Space&gt;   ; ƒ function
:inoremap &lt;A--&gt; &lt;C-v&gt;u2192&lt;Space&gt;  ; → right arrow
:inoremap &lt;A-a&gt; &lt;C-v&gt;u03b1&lt;Space&gt;  ; α alpha
:inoremap &lt;A-y&gt; &lt;C-v&gt;u03c8&lt;Space&gt;  ; ψ psi</code></pre>

<h2>Mac OS X keybindings</h2>

<p>Add to `~/Library/KeyBindings/DefaultKeyBinding.dict&apos;</p>

<pre><code>{
"~l" = ("insertText:", "\U03BB"); /* alt + l ~&gt; λ lambda */
"~f" = ("insertText:", "\U0192"); /* alt + f ~&gt; ƒ function */
"~-" = ("insertText:", "\U2192"); /* alt + - ~&gt; → right arrow */
"~a" = ("insertText:", "\U03b1"); /* alt + a ~&gt; α alpha */
"~y" = ("insertText:", "\U03c8"); /* alt + y ~&gt; ψ psi */
}</code></pre>

<h2>StumpWM keybindings</h2>

<pre><code><span class="code"><span class="paren1">(<span class="code"><i><span class="symbol">defmacro</span></i> <i><span class="symbol">defkeys-top</span></i> <span class="paren2">(<span class="code">&amp;rest keys</span>)</span>
  <span class="paren2">(<span class="code"><i><span class="symbol">let</span></i> <span class="paren3">(<span class="code"><span class="paren4">(<span class="code">ks <span class="paren5">(<span class="code">mapcar #'<span class="paren6">(<span class="code"><i><span class="symbol">lambda</span></i> <span class="paren1">(<span class="code">k</span>)</span> <span class="paren1">(<span class="code">cons '<i><span class="symbol">defkey-top</span></i> k</span>)</span></span>)</span> keys</span>)</span></span>)</span></span>)</span>
    `<span class="paren3">(<span class="code"><i><span class="symbol">progn</span></i> ,@ks</span>)</span></span>)</span></span>)</span>

<span class="paren1">(<span class="code"><i><span class="symbol">defcommand</span></i> xdo-lambda <span class="paren2">(<span class="code"></span>)</span> <span class="paren2">(<span class="code"></span>)</span>
    <span class="paren2">(<span class="code">run-shell-command <span class="string">"xdotool type λ"</span></span>)</span></span>)</span>
<span class="paren1">(<span class="code"><i><span class="symbol">defcommand</span></i> xdo-fn <span class="paren2">(<span class="code"></span>)</span> <span class="paren2">(<span class="code"></span>)</span>
    <span class="paren2">(<span class="code">run-shell-command <span class="string">"xdotool type ƒ"</span></span>)</span></span>)</span>
<span class="paren1">(<span class="code"><i><span class="symbol">defcommand</span></i> xdo-alpha <span class="paren2">(<span class="code"></span>)</span> <span class="paren2">(<span class="code"></span>)</span>
    <span class="paren2">(<span class="code">run-shell-command <span class="string">"xdotool type α"</span></span>)</span></span>)</span>
<span class="paren1">(<span class="code"><i><span class="symbol">defcommand</span></i> xdo-arrow <span class="paren2">(<span class="code"></span>)</span> <span class="paren2">(<span class="code"></span>)</span>
    <span class="paren2">(<span class="code">run-shell-command <span class="string">"xdotool type →"</span></span>)</span></span>)</span>

<span class="paren1">(<span class="code"><i><span class="symbol">defkeys-top</span></i>
    <span class="paren2">(<span class="code"><span class="string">"s-l"</span> <span class="string">"xdo-lambda"</span></span>)</span>
    <span class="paren2">(<span class="code"><span class="string">"s-f"</span> <span class="string">"xdo-fn"</span></span>)</span>
    <span class="paren2">(<span class="code"><span class="string">"s--"</span> <span class="string">"xdo-arrow"</span></span>)</span>
    <span class="paren2">(<span class="code"><span class="string">"s-a"</span> <span class="string">"xdo-alpha"</span></span>)</span></span>)</span></span></code></pre>

<p>If you&apos;re absolutely opposed to non-ascii characters, you can use:
<code>lisp
λ == /.
ƒ == f
→ == -&gt;
</code></p>

<h1>License</h1>

<p>See LICENSE.md</p>
