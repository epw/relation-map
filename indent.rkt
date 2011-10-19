#lang racket

(define indentation (make-parameter 0))

(define (indented string-to-indent)
  (for ((i (in-range (indentation))))
       (display "\t"))
  (display string-to-indent)
  (display "\n"))

(define-syntax-rule (indent exp ...)
  (parameterize ((indentation (add1 (indentation))))
    exp ...))

(provide/contract
 (indented (-> string? any)))

(provide indentation indent)
