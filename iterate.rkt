#lang racket

;; Code for the book Chaos, Fractals, and Dynamics: Computer Experiments in Mathematics.
;; 

#|
(require 2htdp/image) ; draw a picture
(define (sierpinski [n 8])
  (cond
    [(zero? n) (triangle 2 'solid 'red)]
    [else (define t (sierpinski (- n 1)))
          (freeze (above t (beside t t)))]))
|#

(require racket/gui/base
         racket/flonum
         racket/fixnum)

#|
;; this bit of code replaces the standard flonum functions with their unsafe versions,
;; but I don't see a performance improvement when using it.

(require racket/require racket/require-syntax)

(define-require-syntax overriding-in
  (syntax-rules () [(_ R1 R2) (combine-in R2 (subtract-in R1 R2))]))

(require (overriding-in
          racket/flonum
          (filtered-in (lambda (name) (regexp-replace #rx"unsafe-" name ""))
                       racket/unsafe/ops)))
|#

(struct range (min max)
        #:transparent)

(define (update-range r value)
  (cond 
    [(< value (range-min r)) (range value (range-max r))]
    [(> value (range-max r)) (range (range-min r) value)]
    [#t r]))

(define (make-logistic-func c)
  (lambda (x) (* c x (- 1 x))))

(define (make-quadratic-func c)
  (lambda (x) (+ (* x x) c))
  #;(lambda (x)
    (define rx (flreal-part x))
    (define ix (flimag-part x))
    (+ (make-flrectangular
        (fl- (fl* rx rx) (fl* ix ix))
        (fl+ (fl* rx ix) (fl* rx ix)))
       c))
  )

(define (iterate f x i)
  (if (= i 1) (f x)
      (iterate f (f x) (- i 1))))

(define (iterate-print f x i)
  (define (iterate-print-helper x counter)
    (let ([result (f x)])
      (printf "~a: ~a~n" counter result)
      (if (= counter i) result
          (iterate-print-helper result (+ counter 1)))))

  (iterate-print-helper x 1))

(define (iterate-plot f x i)
  (define target (make-bitmap 300 300))
  (define dc (new bitmap-dc% [bitmap target]))

  (define (iterate-plot-helper x counter)
    (define result (f x))
    (send dc set-pixel (exact-round (* 300 result)) 100 (make-color 255 0 0))
    (if (= counter i) 
        (send dc get-bitmap)
        (iterate-plot-helper result (+ counter 1))))
  
  (iterate-plot-helper x 1))

;; ex: (iterate-complex-plot (lambda (x) (* x x)) (make-rectangular 0.6 0.8) 50)
(define (iterate-complex-plot f x i)
  (define target (make-bitmap 300 300))
  (define dc (new bitmap-dc% [bitmap target]))
  ; range of -2 to 2
  (define x-scale (/ 300 4))
  (define y-scale (* -1 (/ 300 4)))
  (define x-origin (/ 300 2))
  (define y-origin (/ 300 2))

  (define (iterate-plot-helper x counter)
    (define result (f x))
    (send dc set-pixel (exact-round (+ x-origin 
                                       (* x-scale (real-part result))))
                       (exact-round (+ y-origin 
                                       (* y-scale (imag-part result))))
                       (make-color 255 0 0))
    (if (= counter i) 
        (send dc get-bitmap)
        (iterate-plot-helper result (+ counter 1))))
  
  (iterate-plot-helper x 1))

;; creates a window that calls draw-func to paint itself
(define (draw-something draw-func)
  (define frame (new frame%
                     [label "Example"]
                     [width 300]
                     [height 300]))
  (new canvas% [parent frame]
       [paint-callback
        (lambda (canvas dc)
          (draw-func dc))])
  (send frame show #t))

(define (draw-text dc)
  (send dc set-scale 3 3)
  (send dc set-text-foreground "blue")
  (send dc draw-text "Don't Panic!" 0 0))

;; define a canvas that displays a bitmap when its on-paint
;; method is called
(define bitmap-canvas%
  (class canvas%
    (init-field [bitmap #f])
    (inherit get-dc)
    (define/override (on-paint)
      (send (get-dc) draw-bitmap bitmap 0 0))
    (super-new)))

;; create a window that displays bitmap
(define (show-bitmap bitmap w h)
  (define frame (new frame%
                     [label "Bitmap"]
                     [width w]
                     [height h]))
  (new bitmap-canvas% [parent frame] [bitmap bitmap])
  (send frame show #t))

;; sample usage:
;;(show-bitmap (iterate-plot sin -30 10))

;; returns a simple bitmap
(define (test-bitmap)
  (define target (make-bitmap 300 300))
  (define dc (new bitmap-dc% [bitmap target]))
  (send dc set-pixel 10 10 (make-color 255 0 0))
  (send dc get-bitmap))

;; plots the function f on the coordinate system ranging from - x-axis-length/2
;; to x-axis-length/2. Assume the drawing context dc has width pixels.
(define (draw-plot dc f x-axis-length width)
  (define xmax (/ x-axis-length 2))
  (define x-increment (/ x-axis-length width)) 
  (let ([p (new dc-path%)])
    (send p move-to (- xmax) (f (- xmax)))
    (for ([x (in-range (- xmax) xmax x-increment)])
      (send p line-to x (f x)))
    (send dc set-pen "red" (/ x-axis-length width) 'solid)
    (send dc set-brush "white" 'transparent)
    (send dc draw-path p)))

(define (draw-graphical-analysis dc f x x-axis-length width)
  (send dc set-pen "blue" (/ x-axis-length width) 'solid)
  (define (draw-graphical-analysis-helper x counter)
    (define f-of-x (f x))
    (cond 
      [(= counter 0) #t]
      [(= x f-of-x) #t]
      [else
       (send dc draw-line x x x f-of-x)
       (send dc draw-line x f-of-x f-of-x f-of-x)
       (draw-graphical-analysis-helper f-of-x (- counter 1))]))
          
  (draw-graphical-analysis-helper x 10))

;; f is the function to plot, x0 is the initial x for orbit analysis
;; x/y-axis-length are the lengths of the cartessian coordinate system axes to show eg. abs(-x) + x 
;; width and height are the dimensions of the bitmap returned
;; ex: (show-bitmap (plot-orbit-to-bitmap (make-logistic-func 1.5) -0.01 2 2 600 600) 600 600)
(define (plot-orbit-to-bitmap f x0 x-axis-length y-axis-length width height)
  (define (draw-axes dc x-axis-length y-axis-length)
    (define xmax (/ x-axis-length 2))
    (define xmin (- xmax))
    (define ymax (/ y-axis-length 2))
    (define ymin (- ymax))
    
    (send dc draw-line xmin 0 xmax 0)
    (send dc draw-line 0 ymin 0 ymax))
  
  (define (draw-y-eq-x dc x-axis-length y-axis-length)
    (define xmax (/ x-axis-length 2))
    (define xmin (- xmax))
    (define ymax (/ y-axis-length 2))
    (define ymin (- ymax))
    
    (send dc draw-line xmin ymin xmax ymax))

  (define target (make-bitmap width height))
  (define dc (new bitmap-dc% [bitmap target]))
  
  (send dc set-smoothing 'smoothed)
  (send dc set-origin (/ width 2) (/ height 2))
  (send dc set-scale (/ width x-axis-length) (- (/ height y-axis-length)))
  (send dc set-pen "black" (/ x-axis-length width) 'solid)
  
  (draw-axes dc x-axis-length y-axis-length)
  (draw-y-eq-x dc x-axis-length y-axis-length)
  (draw-plot dc f x-axis-length width)
  (draw-graphical-analysis dc f x0 x-axis-length width)

  (send dc get-bitmap))

;; f-class is a function that takes c as an argument and returns the function f we are evaluating
;; x0 is the initial value of x for f(x)
;; num-iter is the number of iterations for each orbit
;; show-after is the number of iterations before we plot the results
;; c-range is the min and max value of c
;; x-width and y-height is the size of the output bitmap
;; example: (orbit-diagram make-quadratic-func 0 200 50 (range -2 0.25) 300 300)
(define (orbit-diagram f-class x0 num-iter show-after c-range x-width y-height)
  (define target (make-bitmap x-width y-height))
  (define dc (new bitmap-dc% [bitmap target]))
  (define plot-color (make-color 255 0 0))
  
  (define c-step (/ (- (range-max c-range) (range-min c-range)) x-width))
  (define y-range (range 0 0))

  ; set the y range based on the range of values from the minimum c value
  ; i don't really like the set! calls, but oh well
  (let ([f (f-class (range-min c-range))]
        [value x0])
    (for ([i num-iter])
      (set! value (f value))
      (set! y-range (update-range y-range value))))

  (define y-min (range-min y-range))
  (define y-max (range-max y-range))
  (define y-diff (- y-max y-min))
  
  (for ([c (in-range (range-min c-range) (range-max c-range) c-step)]
        [x x-width])
    (let ([f (f-class c)]
          [value x0])
      (for ([i num-iter])
        (set! value (f value))
        (if (> i show-after)
            (send dc 
                  set-pixel 
                  x 
                  (- y-height
                     (exact-round (* (/ (- value y-min) y-diff)
                                     y-height)))
                  plot-color)
            #f))))

  (send dc get-bitmap))

;; z is a complex number
(define (orbit-escapes f escape-value before-iter initial-z)
  (let loop ([z initial-z]
             [i 0])
    (cond [(fl> (magnitude z) escape-value) #t]
          [(>= i before-iter) #f]
          [else (loop (f z) (+ i 1))])))

;; ex: (plot-julia-set (make-quadratic-func (make-rectangular -1 0)) 20 2.0 4 4 200 200)
(define (plot-julia-set f escape-iter escape-magnitude x-axis-length y-axis-length width height)
  (define target (make-bitmap width height))
  (define dc (new bitmap-dc% [bitmap target]))
  (define fill-color (make-color 0 0 0))
  (define back-color (make-color 255 255 255))
  
  (define x-scale (/ x-axis-length width))
  (define y-scale (/ y-axis-length height))

  (printf "Calculating~n")
  
  (for* ([x (in-range width)]
         [y (in-range height)])

    ;(printf ".")

    ; using make-flrectangular makes a huge performance difference with some julia sets
    (if (orbit-escapes f escape-magnitude escape-iter 
                       (make-flrectangular (+ (- (/ x-axis-length 2.0))
                                              (* x x-scale))
                                           (- (/ y-axis-length 2.0)
                                              (* y y-scale))))
        (send dc set-pixel x y back-color)
        (send dc set-pixel x y fill-color)))
  
  (send dc get-bitmap))

;; ex: (plot-mandlebrot-set 30 2.0 4 4 300 300)
(define (plot-mandlebrot-set escape-iter escape-magnitude x-axis-length y-axis-length width height)
  (define target (make-bitmap width height))
  (define dc (new bitmap-dc% [bitmap target]))
  (define fill-color (make-color 0 0 0))
  (define back-color (make-color 255 255 255))
  
  (define x-scale (real->double-flonum (/ x-axis-length width)))
  (define y-scale (real->double-flonum (/ y-axis-length height)))
  (define x-length (real->double-flonum (/ x-axis-length 2.0)))
  (define y-length (real->double-flonum (/ y-axis-length 2.0)))
  
  (printf "Calculating~n")
  
  (for* ([x (in-range width)]
         [y (in-range height)])

    ;(printf ".")

    ; using make-flrectangular makes a huge performance difference with some sets
    (if (orbit-escapes (make-quadratic-func (make-flrectangular (fl+ (fl- x-length)
                                                                     (fl* (->fl x) x-scale))
                                                                (fl- y-length
                                                                     (fl* (->fl y) y-scale))))
                       escape-magnitude 
                       escape-iter 
                       (make-flrectangular 0.0 0.0))
        (send dc set-pixel x y back-color)
        (send dc set-pixel x y fill-color)))
  
  (send dc get-bitmap))

(define mandlebrot-frame% 
  (class frame%

    (init-field [x-axis-length 4])
    (init-field [y-axis-length 4])

    (super-new)
    
    (inherit get-width)
    (inherit get-height)

    (define/override (on-subwindow-event receiver event)
      (define type-of-event (send event get-event-type))

      (cond [(eq? type-of-event 'left-down)
             (define x (send event get-x))
             (define y (send event get-y))
             (define x-scale (/ x-axis-length (get-width)))
             (define y-scale (/ y-axis-length (get-height)))

             (define f (make-quadratic-func (make-flrectangular (+ (- (/ x-axis-length 2.0))
                                                                   (* x x-scale))
                                                                (- (/ y-axis-length 2.0)
                                                                   (* y y-scale)))))
             (show-bitmap
              (plot-julia-set f 20 2.0 4 4 (get-width) (get-height)) 
              ;(orbit-diagram make-quadratic-func 0 200 50 (range -2 0.25) (get-width) (get-height))
              (get-width)
              (get-height))]
            [else #f]))
    
    ))

    


(define (show-mandlebrot w h)
  (define frame (new mandlebrot-frame%
                     [label "Mandlebrot Set"]
                     [width w]
                     [height h]))

  (define bitmap (plot-mandlebrot-set 30 2.0 4 4 w h))

  (new bitmap-canvas% [parent frame] [bitmap bitmap])
  (send frame show #t))
