#lang racket/base
;; Copyright (C) Eric Willisson 2011
;; This library uses the GNU GPL v3.0 or greater
;; see http://www.gnu.org/copyleft/gpl.html for details

;; This is a simple module which allows lines to be printed out with
;; "the right amount of indentation."

(require racket/contract)

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
