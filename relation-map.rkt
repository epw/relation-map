#lang racket/base
(require racket/list)
(require racket/contract)

(require "indent.rkt")

(struct node (name shape))

(struct edge (node1 node2 color style dir))

(struct section (name (nodes #:mutable) (edges #:mutable)))

(define (get-none) (last (graph)))
(define-syntax none
  (syntax-id-rules ()
		   ((none a ...) ((get-none) a ...))
		   (none (get-none))))

(define (new-node name shape (place (first (graph))))
  (let ((the-node (node name shape)))
    (set-section-nodes! place (cons the-node (section-nodes place)))
    the-node))

(define (new-edge node1 node2 color style (dir null))
  (let ((the-edge (edge (get-node node1) (get-node node2) color style
			(if (eq? dir 'null) null dir))))
    (set-section-edges! none (cons the-edge (section-edges none)))
    the-edge))

(define (new-section name)
  (graph (cons (section name empty empty) (graph))))

(define (new-graph) 
  (list (section null empty empty)))

(define graph (make-parameter (new-graph)))

(define (all-nodes)
  (define (iter node-list the-graph)
    (if (empty? the-graph) node-list
	(iter (append (section-nodes (first the-graph)) node-list)
	      (rest the-graph))))
  (iter empty (graph)))

(define (get-node node-identifier)
  (cond ((node? node-identifier) node-identifier)
	((string? node-identifier)
	 (first
	  (filter (lambda (a-node) (string=? node-identifier
					     (node-name a-node)))
		  (all-nodes))))))

(define (safe-string a-string)
  (regexp-replace* "[^a-zA-Z0-9_]" a-string "_"))

(define (output-graph)
  (display "graph G {\n")
  (indent
   (indented "rankdir = \"LR\";")
   (for ((a-section (graph)))
	(output-section a-section)))
  (display "}\n"))

(define (url identifier)
  (format "nodes/~a.html" identifier))

(define (node-representation a-node)
  (format "\"~a\"" (node-name a-node)))

(define (output-node a-node)
  (indented (format "~a [shape=~a, URL=\"~a\"];" (node-representation a-node)
		    (node-shape a-node)
		    (url (safe-string (node-name a-node))))))

(define (dir-string-if-present dir-string)
  (if (not (null? dir-string))
      (format ", dir=~a" dir-string)
      ""))

(define (output-edge an-edge)
  (indented (format "~a -- ~a [color=~a, style=~a~a];"
		    (node-representation (edge-node1 an-edge))
		    (node-representation (edge-node2 an-edge))
		    (edge-color an-edge) (edge-style an-edge)
		    (dir-string-if-present (edge-dir an-edge)))))

(define (handle-section a-section)
  (for ((a-node (reverse (section-nodes a-section))))
       (output-node a-node))
  (for ((an-edge (reverse (section-edges a-section))))
       (output-edge an-edge)))

(define (output-section a-section)
  (if (not (null? (section-name a-section)))
      (begin
	(indented (format "subgraph cluster_~a {"
			  (safe-string (section-name a-section))))
	(indent
	 (indented (format "label = ~s;" (section-name a-section)))
	 (indented "color=gray;")
	 (handle-section a-section))
	(indented "}"))
  (handle-section a-section)))

(define-syntax-rule (node-type new-type shape)
  (define (new-type name (place (first (graph))))
    (new-node name 'shape place)))

(define-syntax rule
  (syntax-rules ()
    ((rule new-type color) (rule new-type color solid null))
    ((rule new-type color style) (rule new-type color style null))
    ((rule new-type color style dir)
     (define (new-type node1 node2)
       (new-edge node1 node2 'color 'style 'dir)))))

(define dsls (make-parameter (list "plot.def")))

(define (use definitions)
  (if (member definitions (dsls))
      (load definitions)
      (display (format "Error: definitions file ~a not accepted.~%"
		       definitions) (current-error-port))))

(define-syntax-rule (label object identifier)
  (define identifier object))

(provide node-type rule none use label output-graph)
(provide/contract
 (new-section (-> string? any)))
