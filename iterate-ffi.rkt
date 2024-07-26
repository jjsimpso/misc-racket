#lang racket

(require racket/gui/base
         racket/flonum
         racket/fixnum
         ffi/unsafe
         ffi/unsafe/define)

(define-ffi-definer define-iterate (ffi-lib "libiterate"))

(define-cstruct _complex_num ([r _double]
                              [i _double]))

(define-iterate set_quad_c (_fun _complex_num -> _void))
(define-iterate mag (_fun _complex_num-pointer -> _double))
(define-iterate quad_func (_fun _complex_num-pointer -> _complex_num))

;; z is a complex number
(define (orbit-escapes f escape-value before-iter initial-z)
  (let loop ([z initial-z]
             [i 0])
    (cond [(fl> (mag z) #;(flsqrt (fl+ (fl* (complex_num-r z) (complex_num-r z))
                             (fl* (complex_num-i z) (complex_num-i z))))
                escape-value) #t]
          [(>= i before-iter) #f]
          [else (loop (f z) (+ i 1))])))

;; ex: (plot-mandlebrot-set 30 2.0 4 4 300 300)
(define (plot-mandlebrot-set escape-iter escape-magnitude x-axis-length y-axis-length width height)
  (define target (make-bitmap width height))
  (define dc (new bitmap-dc% [bitmap target]))
  (define fill-color (bytes 255 0 0 0));(make-color 0 0 0))
  (define back-color (bytes 255 255 255 255));(make-color 255 255 255))

  (define x-scale (real->double-flonum (/ x-axis-length width)))
  (define y-scale (real->double-flonum (/ y-axis-length height)))
  (define x-length (real->double-flonum (/ x-axis-length 2.0)))
  (define y-length (real->double-flonum (/ y-axis-length 2.0)))
  (define argb-pixels (make-bytes (* width height 4) 0))
  (define row-bytes (* width 4))
  (define initial-z (make-complex_num 0.0 0.0))
  
  (printf "Calculating~n")
  
  (for* ([x (in-range width)]
         [y (in-range height)])
    (set_quad_c (make-complex_num (fl+ (fl- x-length)
                                       (fl* (->fl x) x-scale))
                                  (fl- y-length
                                       (fl* (->fl y) y-scale))))
    (if (orbit-escapes quad_func
                       escape-magnitude 
                       escape-iter 
                       initial-z)
        (bytes-copy! argb-pixels (+ (* y row-bytes) (* x 4)) back-color 0 4)
        (bytes-copy! argb-pixels (+ (* y row-bytes) (* x 4)) fill-color 0 4)))
  
  (send dc set-argb-pixels 0 0 width height argb-pixels)  
  (send dc get-bitmap))
