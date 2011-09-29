#! /usr/bin/env racket
#lang racket

(struct plot (name nodes))

(struct node (name shape))
(define (make-actor name)
  (node name "box"))

(define (make-plot name . nodes)
  (plot name nodes))
