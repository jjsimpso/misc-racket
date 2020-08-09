#lang racket

;; https://github.com/racket/racket/issues/3344

(define ht (make-hash))

(for ([i 64])
  (hash-set! ht i i))

(hash-copy ht)
