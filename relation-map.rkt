#lang racket/base

(require xml
	 racket/cmdline
	 racket/system
	 racket/contract
	 racket/file
	 racket/port)

(require "parameters.rkt")

(require (prefix-in write-dot-file: "write-dot-file.rkt"))

(define create-files (make-parameter #f))

(define (display-doctype (out-port (current-output-port)))
  (display "<!DOCTYPE html>" out-port))

(define (dot-path-with-suffix path suffix)
  (path->string (path-replace-suffix path suffix)))

(define (print-and-return s)
  (display (format "~a~%" s) (current-error-port))
  s)

(define (dot-string->map dot-string)
  (let ((map-string ""))
    (let-values (((p-stdout p-stdin pid p-stderr control)
		  (apply values (process
				 (format "dot -Tpng -o~a -Tcmapx"
					 (if (string=?
					      (write-dot-file:output-filename)
					      "")
					     "map.png"
					     (dot-path-with-suffix
					      (write-dot-file:output-filename)
					      "map.png")))))))
      (display dot-string p-stdin)
      (close-output-port p-stdin)
      (control 'wait)
      (set! map-string (port->string p-stdout))
      (let ((err (port->string p-stderr)))
	(unless (string=? err "")
	  (display err (current-error-port))))
      (close-input-port p-stdout)
      (close-input-port p-stderr))
    map-string))

(define (not-void? obj)
  (not (void? obj)))

(define (maybe-create-files xexpr)
  (when (create-files)
    (for ((area (filter not-void?
			(map (lambda (cell)
			       (when (list? cell)
				 (cddadr cell)))
			     (cdddr xexpr)))))
	 (with-handlers
	     ((exn:fail:filesystem? void))
	   (call-with-output-file (cadar area)
	     (lambda (out)
	       (display-doctype out)
	       (display-xml/content
		(xexpr->xml
		 `(html
		   (head
		    (title ,(cadar (cdddr area)))
		    (link ((rel "stylesheet") (type "text/css")
			   (href "style.css"))))
		   (body
		    (h1 ,(cadar (cdddr area))))))
		out))
	     #:exists 'error))))
  xexpr)

(define (make-map-page out-port name map-string)
  (display-doctype out-port)
  (display-xml/content (xexpr->xml
			`(html
			  (head
			   (title "Relation Map")
			   (link ((rel "stylesheet") (type "text/css")
				  (href "style.css"))))
			  (body
			   (img ((src ,(dot-path-with-suffix name "map.png"))
				 (border "0") (usemap "#G")))
			   ,(maybe-create-files (string->xexpr map-string)))))
		       out-port))

(define (render-dot-file dot-path dot-string)
  (call-with-output-file (dot-path-with-suffix dot-path ".html")
    (lambda (out) (make-map-page out dot-path (dot-string->map dot-string)))
    #:exists 'truncate)
  (dot-path-with-suffix dot-path ".html"))

(define (render-dot-string dot-string)
  (let ((out (open-output-string)))
    (make-map-page out ".plot" (dot-string->map dot-string))
    (get-output-string out)))

(define (main)
  (let ((filename
	 (command-line
	  #:program "relation-map.rkt"
	  #:once-each
	  (("-c" "--create") "Create files referenced in node links."
	   (create-files #t))
	 (("-p" "--pred") pred "URL prefix string for node links in image map"
	  (url-predicate pred))
	 #:multi
	 (("-d" "--definitions") path "Definition file or directory available to maps"
	  (write-dot-file:allow-definitions path))
	 (("-D" "--default-definitions") path "Definition file to be automatically used in maps"
	  (write-dot-file:allow-definitions path)
	  (default-definitions (cons path (default-definitions))))
	 #:args ((filearg "-"))
	 filearg)))
    (parameterize ((current-output-port (open-output-string)))
      (write-dot-file:main filename)
      (if (string=? (write-dot-file:output-filename) "")
	  (render-dot-string (get-output-string (current-output-port)))
	  (render-dot-file (write-dot-file:output-filename)
			   (get-output-string (current-output-port)))))))

(provide/contract
; (dot-file->map (-> string? string?))
 (dot-string->map (-> string? string?))
 (make-map-page (-> output-port? string? string? any))
 (render-dot-file (-> string? string? string?))
 (render-dot-string (-> string? string?))
 (main (-> string?)))
