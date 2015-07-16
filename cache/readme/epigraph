
epigraph, a library for manipulating graphs in common lisp.
Cyrus Harmon <ch-lisp@bobobeach.com>

Epigraph is a common lisp package for representing graph, that is sets
of nodes that may contain edges between the various nodes.

# Example

(defparameter *graph* 
  (let ((g (make-graph :node-test 'equal)))

    (loop for i from 1 below 12
       do (add-node g (concatenate 'string "Node " (princ-to-string i))))

    (add-edge-between-nodes g "Node 1" "Node 2")
    (add-edge-between-nodes g "Node 1" "Node 3")
    (add-edge-between-nodes g "Node 3" "Node 4")
    (add-edge-between-nodes g "Node 3" "Node 5")
    (add-edge-between-nodes g "Node 2" "Node 6")
    (add-edge-between-nodes g "Node 6" "Node 7")
    (add-edge-between-nodes g "Node 6" "Node 8")
    (add-edge-between-nodes g "Node 4" "Node 9")
    (add-edge-between-nodes g "Node 5" "Node 10")
    (add-edge-between-nodes g "Node 5" "Node 11")
    (add-edge-between-nodes g "Node 1" "Node 11")
    (add-edge-between-nodes g "Node 8" "Node 7")
    g))
