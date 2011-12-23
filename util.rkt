#lang racket/base

(require racket/contract)

(define (read-all-lines-from-file filename)
  (call-with-input-file filename read-all-lines))

(define (read-all-lines input-file)
  (define (iter input-file accumulated-string-list)
    (let ((line (read-line input-file)))
      (if (eq? line eof) accumulated-string-list
          (iter input-file (cons line accumulated-string-list)))))
  (reverse (iter input-file null)))

(provide/contract
 (read-all-lines (-> input-port? (listof string?)))
 (read-all-lines-from-file (-> string? (listof string?))))
