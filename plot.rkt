#lang racket

(require "relation-map.rkt")

(provide new-section none)

(node-type character box)
(node-type objective ellipse)
(node-type item diamond)
(node-type group octagon)

(rule allies green)
(rule weakly-allied green dashed)
(rule member-of green bold forward)
(rule enemies red)
(rule hates red solid forward)
(rule watching black dashed forward)
(rule seeking blue solid forward)
(rule possesses purple)
(rule owns black solid forward)
(rule controls orange solid forward)

(provide #%top-interaction #%app)
