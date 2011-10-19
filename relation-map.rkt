#! /usr/bin/env racket
#lang racket

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
  (set-section-nodes! place (cons (node name shape)
				  (section-nodes place))))

(define (new-edge node1 node2 color style (dir null))
  (set-section-edges! none (cons (edge (get-node node1) (get-node node2)
				       color style (if (eq? dir 'null) null
						       dir))
				 (section-edges none))))

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

(define (get-node name)
  (first
   (filter (lambda (a-node) (string=? name (node-name a-node))) (all-nodes))))

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

(define-syntax edge-type
  (syntax-rules ()
    ((edge-type new-type color style) (edge-type new-type color style null))
    ((edge-type new-type color style dir)
     (define (new-type node1 node2)
       (new-edge node1 node2 'color 'style 'dir)))))

(define (example)
  (parameterize ((graph (list (section null empty empty))))
    (node-type character box)
    (node-type organization octagon)
    (edge-type knows black solid forward)

    (new-section "All Pathwalkers")
    (character "Ged")
    (organization "Black Star Guild")
    (character "Iawen" none)
    (knows "Ged" "Iawen")
    (output-graph)))
