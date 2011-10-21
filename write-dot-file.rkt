#! /usr/bin/env racket
#lang racket/base
(require racket/cmdline)

(require "relation-map.rkt")
(require "definition-base.rkt")



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

(define allowed-definitions (list))

(define (include-definitions definitions)
  (if (directory-exists? definitions)
      (set! allowed-definitions (append (map (lambda (path)
					       (format "~a/~a" definitions
						       (path->string path)))
					     (directory-list definitions))))
      (set! allowed-definitions (cons definitions allowed-definitions))))

(define (get-definitions)
  allowed-definitions)

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
	  #:args (filearg)
	  filearg)))
;      (write-dot-file (temporary-file-from-port (current-input-port)))
    (write-dot-file filename)))

(main)
