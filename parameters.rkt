#lang racket/base

(define url-predicate (make-parameter ""))

(define definition-files (make-parameter null))

(provide url-predicate definition-files)
