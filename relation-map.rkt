#lang racket/base

(require xml
	 racket/contract)

(require (prefix-in write-dot-file: "write-dot-file.rkt"))

(define (render-dot-file dot-file)
  (system (format "dot -Tcmapx -omap.html -Tpng -o"${input}map.png" $input"

(define (main)
  (parameterize ((current-output-port (open-output-string)))
    (write-dot-file:main)
    (if (string=? (write-dot-file:output-filename) "")
	(render-dot-file (write-dot-file:output-filename))
	(render-dot-string (get-output-string (current-output-port))))))

(provide/contract
 (render-dot-file (-> string? any))
 (main (-> any any)))
