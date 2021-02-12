#lang racket/base

(define (swap-slot-vals! v x y)
  (when (not (= x y))
    (define tmp (vector-ref v x))
    (vector-set! v x (vector-ref v y))
    (vector-set! v y tmp)))

(define (partition-ver1! v start end)
  ;; increment p until we find an element < p
  (let loop ([p start]
             [cursor (add1 start)])
    (cond
      [(> cursor end) p]
      [(>= (vector-ref v cursor) (vector-ref v p))
       (loop (add1 p) (add1 cursor))]
      [else
       ;; check the rest of the items and insert them to the left of p if they are < p
       (let loop2 ([p p]
                   [cursor cursor])
         (cond
           [(> cursor end) p]
           [(>= (vector-ref v cursor) (vector-ref v p))
            (loop2 p (add1 cursor))]
           [(< (vector-ref v cursor) (vector-ref v p))
            (swap-slot-vals! v (add1 p) cursor)  ; if cursor == p+1 the swap does nothing
            (swap-slot-vals! v p (add1 p))
            (loop2 (add1 p) (add1 cursor))]))])))

(define (partition! v start end)
  (define (find-next v pos end-pos inc op val)
    (cond
      [(op (vector-ref v pos) val) pos]
      [(= pos end-pos) pos]
      [else
       (find-next v (+ pos inc) end-pos inc op val)]))

  (define pv (vector-ref v end))
  
  (printf "entering partition: ~a-~a of ~a~n" start end v)
  
  (let loop ([l start]
             [r end])

    (define i (find-next v l r 1 > pv))
    (define j (find-next v r i -1 < pv))

    (printf "~a ~a~n" i j)

    (swap-slot-vals! v i j)
    (display v) (printf "~n")
    
    (if (< i j)
        (loop i j)
        (begin 
          (swap-slot-vals! v i end)
          (printf "exiting partition: ~a, ~a~n" i v)
          i))))
  
(define (quicksort! v start end)
  (cond
    [(>= start end) void]
    [else
     (define p (partition! v start end))
     (quicksort! v start (sub1 p)) 
     (quicksort! v (add1 p) end)
     v]))
