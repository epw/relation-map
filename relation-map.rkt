#! /usr/bin/env racket
#lang racket

(require "indent.rkt")

(struct node (name shape))

(struct section (name (nodes #:mutable) (edges #:mutable)))

(define new-node
  (case-lambda
    ((name) (new-node name 'box (first (graph))))
    ((name shape) (new-node name shape (first (graph))))
    ((name shape place)
     (set-section-nodes! place (cons (node name shape)
				     (section-nodes place))))))

(define (new-section name)
  (graph (cons (section name empty empty) (graph))))

(define graph (make-parameter (list (section null empty empty))))

(define (get-none) (last (graph)))
(define-syntax none
  (syntax-id-rules ()
		   ((none a ...) ((get-none) a ...))
		   (none (get-none))))

(define (safe-string a-string)
  (regexp-replace* "[^a-zA-Z0-9_]" a-string "_"))

(define (output-graph)
  (display "graph G {\n")
  (indent
   (for ((a-section (graph)))
	(output-section a-section)))
  (display "}\n"))

(define (output-node a-node)
  (indented (format "\"~a\" [shape=~a];" (node-name a-node)
		    (node-shape a-node))))

(define (handle-section a-section)
  (for ((a-node (section-nodes a-section)))
       (output-node a-node)))

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

(define character
  (case-lambda
    ((name) (new-node name 'box))
    ((name place) (new-node name 'box place))))

(define (example)
  (parameterize ((graph (list (section null empty empty))))
    (new-section "All Pathwalkers")
    (character "Ged")
    (character "Iawen" none)
    (output-graph)))
