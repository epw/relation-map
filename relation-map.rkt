#lang racket/base

(require xml
	 racket/contract
	 racket/file)

(require (prefix-in write-dot-file: "write-dot-file.rkt"))

(define (render-dot-file dot-path)
  (system (format "dot -Tcmapx -o~a -Tpng -o~a ~a"
		  (path->string (path-replace-suffix dot-path "map.html"))
		  (path->string (path-replace-suffix dot-path "map.png"))
		  dot-path)))

(define (main)
  (parameterize ((current-output-port (open-output-string)))
    (write-dot-file:main)
    (if (string=? (write-dot-file:output-filename) "")
	(render-dot-file (write-dot-file:output-filename))
	(render-dot-string (get-output-string (current-output-port))))))

(provide/contract
 (render-dot-file (-> string? any))
 (main (-> any any)))
