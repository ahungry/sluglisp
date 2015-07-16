# cl-ca: common lisp cellular automata
cl-ca is a common lisp library for simply implementing programs using cellular
automata (e. g. Conway's game of life).

## usage
There some [example programs](./examples) to introduce you. Also it might help to read the actual source code (don't worry: It isn't too much) and the following chapter here.
(more detailed documentation is about to come)

## architecture
cl-ca is build around three functions which access the hash table all automata are stored in. This hash-table is located in a closure, therefore only these
functions can access it.

The functions are:

* `set-automaton` used to modify/add automata
* `get-automaton` used to get informations about an automaton
	(The Hacker's note: It is just a call to `gethash`)
* `run-step` walks over a specified part of the hash-table and calculates the new states of the automatons.

The data about the automata is stored this way in the hash-table:

	(x . y) . (state function)

(the cons pair `(x . y)` is the key and the list ist the value)

The function gets called each step with the list representing the automaton
itself and a list of lists representing its neighbors (more on that later).
It must return a list representing itself (this one then gets put in the hash-table). This allows an automaton not only change its state but also to modify its behavior.

The functions `moore-neighbors` and `von-neumann-neighbors` implement neighborhood (See the corresponding Wikipedia articles: [Moore Neighborhood](https://en.wikipedia.org/wiki/Moore_neighborhood) and [Von Neumann Neighborhood](https://en.wikipedia.org/wiki/Von_Neumann_neighborhood)). One of these functions (or another one doing something reasonable in this context) should be passed to `run-step` and is used to calculate neighborhood then.

## can I ql:quickload it?
[I think so](https://github.com/quicklisp/quicklisp-projects/tree/master/cl-ca)
