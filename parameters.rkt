#lang racket/base
;; Copyright (C) Eric Willisson 2011
;; This library uses the GNU GPL v3.0 or greater
;; see http://www.gnu.org/copyleft/gpl.html for details

;; This module defines the variable parameters used in the
;; program. Putting them in their own module allows them to be altered
;; by the main program, and still referenced by the sanitized
;; domain-specific language without being editable.
;;
;; This is particularly important in the case of url-predicate, which
;; could be used to write files anywhere in the system if it were
;; public.

(define url-predicate (make-parameter ""))

(define definition-files (make-parameter (make-hash)))

(define default-definitions (make-parameter '()))

(provide url-predicate definition-files default-definitions)
