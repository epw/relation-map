#! /usr/bin/env racket
#lang racket/base
;; Copyright (C) Eric Willisson 2011
;; This library uses the GNU GPL v3.0 or greater
;; see http://www.gnu.org/copyleft/gpl.html for details

;; This module is an executable script which performs the main
;; transformation. It loads all of the other pieces, and then reads a
;; given file (or from stdin) to generate a Graphviz file. Namespaces
;; are used to allow the file it reads to be from any source, without
;; leaving security holes open.

(require racket/cmdline
	 racket/file
	 racket/contract)

(require "output-graph.rkt"
	 "definition-base.rkt")

(define (get-write-line in out)
  (let ((line (read-line in)))
    (unless (eof-object? line)
      (display line out)
      (display "\n" out)
      (get-write-line in out))))

(define output-filename (make-parameter ""))
(define default-definitions (make-parameter '()))

(define (temporary-file-from-port port)
  (let ((filename (make-temporary-file)))
    (call-with-output-file filename
      (lambda (out)
	(get-write-line port out))
      #:exists 'truncate)
    filename))

(define-namespace-anchor a)

(define (write-dot-file in-file)
  (let ((ns (make-base-empty-namespace)))
    (namespace-attach-module (current-namespace) "definition-base.rkt" ns)
    (parameterize ((current-namespace ns))
      (namespace-require "definition-base.rkt")
      (with-handlers ((exn:fail?
		       (lambda (v)
			 (display
			  (format "Error in ~a:~%\t~a~%" in-file (exn-message v))
			  (current-error-port)))))
	(for ((def (default-definitions)))
	     (eval `(use ,def)))
	(load in-file)
	(eval '(output-graph))))))

(define (main)
  (let ((filename
	 (command-line
	  #:program "write-dot-file.rkt"
	  #:once-each
	  (("-u" "--url") pred "URL prefix string for node links in image map"
	   (url-predicate pred))
	  #:multi
	  (("-d" "--definitions") path "Definition file or directory available to maps"
	   (allow-definitions path))
	  (("-D" "--default-definitions") path "Definition file to be automatically used in maps"
	   (allow-definitions path)
	   (default-definitions (cons path (default-definitions))))
	  #:args ((filearg "-"))
	  filearg)))
    (if (string=? filename "-")
	(write-dot-file (temporary-file-from-port (current-input-port)))
	(begin
	  (output-filename filename)
	  (write-dot-file filename)))))

(provide/contract
 (main (-> any))
 (write-dot-file (-> string? any))
 (output-filename (->* () (string?) string?)))
