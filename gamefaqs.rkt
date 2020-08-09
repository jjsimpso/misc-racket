#lang racket

(require html-parsing)
(require sxml)

(define output-directory (make-parameter "/tmp/system/"))

(struct game
  (name
   path-to-faqs)
  #:transparent)
#|
(define (extract-faq-from-html faq-html)
  (define wrapped-faq ((sxpath '(// pre span)) faq-html))
  
  (if (null? wrapped-faq) null
      (cddr (last wrapped-faq)))
  )
|#

(define (extract-faq-from-html* faq-html)
  (define (extract-faq-from-html*-helper span-count faq-html)
    (define faq-span 
      ((sxpath (quasiquote 
                (// (span (@ (equal? (id (unquote (string-append "faqspan-" 
                                                                 (number->string span-count))))))))))
                faq-html))
    (define faq-span-pre 
      ((sxpath (quasiquote 
                (// (pre (@ (equal? (id (unquote (string-append "faqspan-" 
                                                                 (number->string span-count))))))))))
                faq-html))

    (cond 
      [(not (null? faq-span))
       (cons faq-span (extract-faq-from-html*-helper (+ span-count 1) faq-html))]
      [(not (null? faq-span-pre))
       (cons faq-span-pre (extract-faq-from-html*-helper (+ span-count 1) faq-html))]
      [else null]))

  (foldr append 
         '()
         (map (lambda (wrapped-faq)
                (if (null? wrapped-faq) null
                    (cddr (last wrapped-faq))))
              (extract-faq-from-html*-helper 1 faq-html))))

(define (extract-faq-from-html faq-html)
  (define wrapped-faq1 ((sxpath '(// (span (@ (equal? (id "faqspan-1")))))) faq-html))
  (define wrapped-faq2 ((sxpath '(// (span (@ (equal? (id "faqspan-2")))))) faq-html))
  (define wrapped-faq3 ((sxpath '(// (span (@ (equal? (id "faqspan-3")))))) faq-html))
  (define wrapped-faq4 ((sxpath '(// (span (@ (equal? (id "faqspan-4")))))) faq-html))
  (define wrapped-faq5 ((sxpath '(// (span (@ (equal? (id "faqspan-5")))))) faq-html))
  (define wrapped-faq6 ((sxpath '(// (span (@ (equal? (id "faqspan-6")))))) faq-html))
  (define wrapped-faq7 ((sxpath '(// (span (@ (equal? (id "faqspan-7")))))) faq-html))
  (define wrapped-faq8 ((sxpath '(// (span (@ (equal? (id "faqspan-8")))))) faq-html))
  (define wrapped-faq9 ((sxpath '(// (span (@ (equal? (id "faqspan-9")))))) faq-html))
  
  (foldr append 
         '()
         (map (lambda (wrapped-faq)
                (if (null? wrapped-faq) null
                    (cddr (last wrapped-faq))))
              (list wrapped-faq1 
                    wrapped-faq2 
                    wrapped-faq3 
                    wrapped-faq4 
                    wrapped-faq5 
                    wrapped-faq6 
                    wrapped-faq7 
                    wrapped-faq8 
                    wrapped-faq9)))
  )

(define (extract-title-from-html faq-html)
  ;(define wrapped-faq ((sxpath '(// (h2 (@ (equal? (itemprop "headline")))))) faq-html))
  (define wrapped-faq ((sxpath '(// (h2 (@ (equal? (class "title")))))) faq-html))
  (if (null? wrapped-faq) (number->string 99999999) 
      (third (last wrapped-faq))))

(define (extract-author-from-html faq-html)
  ;(define wrapped-faq ((sxpath '(// (h2 (@ (equal? (itemprop "headline")))))) faq-html))
  (define wrapped-faq ((sxpath '(// 
                                 (a 
                                  (@ (equal? (class "contrib1")))
                                  )))
                       faq-html))
  (if (null? wrapped-faq) "by Unknown"
      (last (last wrapped-faq))))

(define (make-faq-filename title author)
  (string-replace (string-replace (string-append title " " author ".txt") " " "-")
                  "/" "_"))

(define (save-faq-to-file name-of-game path-to-faq)
  (define (skip-faq? path-to-faq)
    (define faqfile (path->string path-to-faq))
    (cond [(not (file-exists? path-to-faq)) #t]
          [(string-contains? faqfile "?") #t]
          [else #f]))
  
  (unless (skip-faq? path-to-faq)
    (define faq-html (call-with-input-file path-to-faq
                       html->xexp))

    (define faq-text (extract-faq-from-html* faq-html))
    (define faq-title (extract-title-from-html faq-html))
    (define faq-author (extract-author-from-html faq-html))
    (define faq-filename (make-faq-filename faq-title faq-author))
    
    (printf "  Extracting FAQ from ~a to ~a~n" path-to-faq faq-filename)
    (when (null? faq-text) (printf "  error parsing faq~n"))

    (unless (null? faq-text) 
      (make-directory* (string-append (output-directory) name-of-game))
      (with-handlers ([exn:fail:filesystem? (lambda (v) #t)])
        (call-with-output-file (build-path (string-append (output-directory) name-of-game) faq-filename)
          (lambda (out)
            (for ([line faq-text])
              (display line out))))))))

(define (extract-all-faqs-for-game game-to-save)
  (define (extract-all-faqs-for-game-helper name list-of-faq-files)
    (cond [(null? list-of-faq-files) null]
          [else (save-faq-to-file name (car list-of-faq-files))
                (extract-all-faqs-for-game-helper name (cdr list-of-faq-files))]))

  (define faq-path (game-path-to-faqs game-to-save))
  
  (printf "Extracting FAQs from ~a~n" faq-path)
  (extract-all-faqs-for-game-helper (game-name game-to-save) 
                                    (directory-list faq-path #:build? #t)))

(define (extract-all-faqs list-of-games)
  (cond [(null? list-of-games) null]
        [(void? (car list-of-games))
         (extract-all-faqs (cdr list-of-games))]
        [else (extract-all-faqs-for-game (car list-of-games))
              (extract-all-faqs (cdr list-of-games))]))

(define (extract-game-name line)
  (define matches (regexp-match #px"/\\d+-(.+)/faqs" line))
  (if matches 
      (last matches)
      #f))

(define (find-games-with-faqs in)
  (for/list ([l (in-lines in)])
    (when (directory-exists? l)
      (game (extract-game-name l) l))))

;; dir-list-file is a text file with a list of directories containing FAQs
;; copy-to-dir is a path like "/tmp/ps2/"
(define (find-all-faqs dir-list-file copy-to-dir)
  (parameterize ([output-directory copy-to-dir])
    (define-values (d f r) (split-path dir-list-file))
    (current-directory d)
    (extract-all-faqs (call-with-input-file dir-list-file find-games-with-faqs))))

