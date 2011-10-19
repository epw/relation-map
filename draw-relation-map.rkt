#! /usr/bin/env racket
#lang racket

(require "relation-map.rkt")

(define (draw-relation-map in)
  

(define (main)
  (if (zero? (vector-length (current-command-line-arguments)))
      (draw-relation-map (current-input-port))
      (call-with-input-file (vector-ref (current-command-line-arguments) 0))))

(main)
