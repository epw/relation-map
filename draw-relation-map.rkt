#! /usr/bin/env racket
#lang racket

(require "relation-map.rkt")

(define (get-write-line in out)
  (let ((line (read-line in)))
    (unless (eof-object? line)
      (display line out)
      (display "\n" out)
      (get-write-line in out))))

(define (temporary-file-from-port port)
  (let ((filename (format "/tmp/racket-~a" (current-milliseconds))))
    (call-with-output-file filename
      (lambda (out)
	(get-write-line port out)))
    filename))

(define-namespace-anchor a)

(define (draw-relation-map in-file)
  (parameterize ((current-namespace (make-base-empty-namespace)))
    (namespace-require "plot.rkt")
    (load in-file)
    (eval '(output-graph))))

(define (main)
  (if (zero? (vector-length (current-command-line-arguments)))
      (draw-relation-map (temporary-file-from-port (current-input-port)))
      (draw-relation-map (vector-ref (current-command-line-arguments) 0))))

(main)
