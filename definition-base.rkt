#lang racket/base
;; Copyright (C) Eric Willisson 2011
;; This library uses the GNU GPL v3.0 or greater
;; see http://www.gnu.org/copyleft/gpl.html for details

;; This module sets up the domain-specific language used to draw
;; relation maps. It ensures that only a few functions are available
;; for the file which is directly (load)ed, preventing security holes.

(require relation-map/output-graph)

(provide #%top-interaction #%app #%datum #%top quasiquote)

(provide new-section none use label output-graph node-type rule)
