#lang racket

; helpful to eliminate differences due to contract checking
(#%declare #:unsafe)

(define (vector-remove-index v index)
  (vector-append (vector-take v index) (vector-drop v (add1 index))))

(define (list-remove-index l index)
  (define-values (a b) (split-at l index))
  (append a (cdr b)))

; doesn't actually shrink the size of the vector since i don't want this to allocate
(define (vector-remove-index! v index)
  (vector-copy! v index v (add1 index)))

(define (build-mlist size proc)
  (define (helper acc i)
    (cond
      [(= i size) acc]
      [else
       (helper (mcons (proc i) acc) (add1 i))]))
  (helper '() 0))

; warning: not robust
(define (mlist-remove-index! ml index)
  (define previous (sub1 index))
  (define (helper m i)
    (if (< i previous)
        (helper (mcdr m) (add1 i))
        (set-mcdr! m (mcdr (mcdr m)))))
  (if (equal? index 0)
      (begin
        (set-mcar! ml (mcar (mcdr ml)))
        (set-mcdr! ml (mcdr (mcdr ml))))
      (helper ml 0)))


(define size 100000)
(define remove-size 10000)

(define v (build-vector size (lambda (x) 42)))
(define l (build-list size (lambda (x) 42)))
(define ml (build-mlist size (lambda (x) 42)))

(define (new-index)
  (random (- size remove-size)))


(printf "vector (functional):~n")
(collect-garbage)
(time
 (for ([i remove-size])
   (vector-remove-index v (new-index))))

(printf "list (functional):~n")
(collect-garbage)
(time
 (for ([i remove-size])
   (list-remove-index l (new-index))))

(printf "vector with mutation:~n")
(collect-garbage)
(time
 (for ([i remove-size])
   (vector-remove-index! v (new-index))))

(printf "mutable list:~n")
(collect-garbage)
(time
 (for ([i remove-size])
   (mlist-remove-index! ml (new-index))))
