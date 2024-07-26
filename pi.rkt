#lang racket

(require racket/flonum)
(require racket/unsafe/ops)
(require racket/future)
(require racket/require)

#;(require (filtered-in
          (λ (name)
            (and (regexp-match #rx"^unsafe-fl" name)
                 (regexp-replace #rx"unsafe-" name "")))
          racket/unsafe/ops))

(define nthreads 8)

(define num-steps 100000000)
(define step-size (/ 1.0 num-steps))

;; calculate pi using the integral of (4.0/(1+x^2) dx) from 0 to 1
(define (calc-sum step steps)
  (for/fold ([sum (unsafe-fl+ 0.0)])
            ([i (in-range 0 steps)])
    (unsafe-fl+ sum (unsafe-fl/ 4.0
                                (unsafe-fl+ 1.0 (sqr (unsafe-fl* (unsafe-fl+ (->fl i) 0.5) step)))))))

(define (calc-sum-block step start end)
  (for/fold ([sum (unsafe-fl+ 0.0)])
            ([i (in-range start end)])
    (define x (unsafe-fl* (unsafe-fl+ (->fl i) 0.5) step))
    (unsafe-fl+ sum (unsafe-fl/ 4.0
                                (unsafe-fl+ 1.0 (fl* x x))))))

(define (calc-sum-parallel step steps threads)
  (define block-size (/ steps threads))
  (define fs
    (for/list ([i (in-range threads)])
      (future (lambda () (calc-sum-block step
                                         (* i block-size)
                                         (* (add1 i) block-size))))))
  (for/fold ([sum (unsafe-fl+ 0.0)])
            ([f (in-list fs)])
    (fl+ sum (touch f))))
  
(define sum 0.0)

(printf "running serial pi calc:~n")
(collect-garbage)
(collect-garbage)
(collect-garbage)
(time
 (set! sum (calc-sum step-size num-steps)))
(printf "pi is ~a~n" (* step-size sum))

(printf "running parallel pi calc with ~a futures:~n" nthreads)
(collect-garbage)
(collect-garbage)
(collect-garbage)
(time
 (set! sum (calc-sum-parallel step-size num-steps nthreads)))
(printf "pi is ~a~n" (* step-size sum))
