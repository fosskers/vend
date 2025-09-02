(defpackage simple-graph
  (:use :cl)
  (:export #:graph #:make-graph #:graph-nodes #:graph-edges
           #:add-node! #:add-edge!
           #:leaf? #:leaves
           #:subgraph #:nodes-to #:paths-to #:flip-edges
           #:to-dot #:to-dot-with-stream)
  (:documentation "A very simple Graph library."))

(in-package :simple-graph)

(defstruct graph
  "Hash Tables of nodes and edges."
  (nodes (make-hash-table :test #'equalp) :type hash-table)
  (edges (make-hash-table :test #'equalp) :type hash-table))

(defun add-node! (graph node)
  "Mutably add some NODE data to a GRAPH. Avoids adding the same node twice. Yields
a truthy value when a new node was successfully added, NIL otherwise."
  (unless (gethash node (graph-nodes graph))
    (setf (gethash node (graph-nodes graph)) t)))

#++
(let ((g (make-graph)))
  (add-node! g "A"))

(defun add-edge! (graph from to)
  "Add an edge between two nodes. The edge ids should be `equal' to the
corresponding node values themselves, but nothing enforces this. Edges between
nodes can be added before the nodes themselves exist. Multiple edges between the
same nodes can be added."
  (cond ((not (gethash from (graph-edges graph)))
         (setf (gethash from (graph-edges graph)) (list to)))
        (t (push to (gethash from (graph-edges graph))))))

#++
(let ((g (make-graph)))
  (add-node! g :a)
  (add-node! g :b)
  (add-edge! g :a :b)
  (add-edge! g :a :c)
  (add-edge! g :c :d)
  g)

(defun leaf? (graph node)
  "Is the given NODE a leaf node?"
  (and (gethash node (graph-nodes graph))
       (null (gethash node (graph-edges graph)))))

#++
(let ((g (make-graph)))
  (add-node! g :a)
  (leaf? g :a))

(defun leaves (graph)
  "A list of all the current leaf nodes."
  (let ((ls '()))
    (maphash (lambda (node _)
               (declare (ignore _))
               (when (leaf? graph node)
                 (push node ls)))
             (graph-nodes graph))
    ls))

#++
(let ((g (make-graph)))
  (add-node! g :a)
  (add-node! g :b)
  (add-node! g :c)
  (add-edge! g :a :c)
  (leaves g))

(defun flip-edges (graph)
  "Flip all the graph edges."
  (let ((new (make-graph)))
    (setf (graph-nodes new) (graph-nodes graph))
    (maphash (lambda (node0 nodes0)
               (dolist (node1 nodes0)
                 (add-edge! new node1 node0)))
             (graph-edges graph))
    new))

(defun subgraph (graph from &rest rest)
  "Form a new subgraph made up of all the nodes and edges reachable from some given
starting nodes."
  (let ((sub (make-graph)))
    (labels ((recurse (next)
               (dolist (n next)
                 (when (not (gethash n (graph-nodes sub)))
                   (add-node! sub n)
                   (let ((edges (gethash n (graph-edges graph))))
                     (dolist (edge edges)
                       (add-edge! sub n edge))
                     (recurse edges))))))
      (recurse (remove-if-not (lambda (n) (gethash n (graph-nodes graph)))
                              (cons from rest))))
    sub))

#++
(let ((g (make-graph)))
  (add-node! g :a)
  (add-node! g :b)
  (add-node! g :c)
  (add-node! g :d)
  (add-node! g :e)
  (add-edge! g :a :c)
  (add-edge! g :d :e)
  (subgraph g :a))

(defun nodes-to (graph node)
  "All nodes with a directed edge to a given NODE."
  (with-hash-table-iterator (iter (graph-edges graph))
    (labels ((recurse (acc)
               (multiple-value-bind (entry-p n edges) (iter)
                 (cond ((not entry-p) acc)
                       ((member node edges :test #'equalp) (recurse (cons n acc)))
                       (t (recurse acc))))))
      (recurse '()))))

#++
(let ((g (make-graph)))
  (add-node! g :a)
  (add-node! g :b)
  (add-node! g :c)
  (add-node! g :d)
  (add-edge! g :a :c)
  (add-edge! g :a :b)
  (add-edge! g :b :d)
  (add-edge! g :c :d)
  (nodes-to g :d))

(defun paths-to (graph node)
  "Find all paths that lead to NODE."
  (let ((to (nodes-to graph node)))
    (cond ((null to) (list (list node)))
          (t (apply #'append
                    (mapcar (lambda (n)
                              (mapcar (lambda (path) (cons node path))
                                      (paths-to graph n)))
                            to))))))

#++
(let ((g (make-graph)))
  (add-node! g :a)
  (add-node! g :b)
  (add-node! g :c)
  (add-node! g :d)
  (add-edge! g :a :c)
  (add-edge! g :a :b)
  (add-edge! g :b :d)
  (add-edge! g :c :d)
  (paths-to g :d))

(defun to-dot-with-stream (graph stream)
  "Write the GRAPH in dot format to some STREAM."
  (let ((curr 0)
        (ixs (make-hash-table :test #'equalp)))
    (format stream "graph {~%")
    (maphash (lambda (node _)
               (declare (ignore _))
               (format stream "  ~a [label=\"~a\", shape=rectangle];~%" curr node)
               (setf (gethash node ixs) curr)
               (incf curr))
             (graph-nodes graph))
    (maphash (lambda (node0 edges)
               (let ((node0-id (gethash node0 ixs)))
                 (dolist (node1 edges)
                   (let ((node1-id (gethash node1 ixs)))
                     (format stream "  ~a -- ~a;~%" node0-id node1-id)))))
             (graph-edges graph))
    (format stream "}")))

(defun to-dot (graph)
  "Write the GRAPH to dot format as a string."
  (with-output-to-string (stream)
    (to-dot-with-stream graph stream)))

#+nil
(let ((g (make-graph)))
  (add-node! g "A")
  (add-node! g "A")
  (add-node! g "A")
  (add-node! g "B")
  (add-node! g "C")
  (add-node! g "D")
  (add-node! g "E")
  (add-node! g "F")
  (add-edge! g "A" "B")
  (add-edge! g "A" "C")
  (add-edge! g "B" "D")
  (add-edge! g "C" "D")
  (add-edge! g "E" "F")
  (to-dot g))

#++
(let ((g (make-graph)))
  (add-node! g "A")
  (add-node! g "A")
  (add-node! g "A")
  (add-node! g "B")
  (add-node! g "C")
  (add-node! g "D")
  (add-node! g "E")
  (add-node! g "F")
  (add-edge! g "A" "B")
  (add-edge! g "A" "C")
  (add-edge! g "B" "D")
  (add-edge! g "C" "D")
  (add-edge! g "E" "F")
  (with-open-file (stream #p"deps.dot" :direction :output :if-exists :supersede)
    (to-dot-with-stream (subgraph g "A") stream)))
