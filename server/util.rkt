#lang racket

(provide 
 (all-defined-out))


(define (->sym v)
  (string->symbol (format "~a" v)))

;; CONTRACT :: (list-of any) any -> (list-of any)
;; Intersperses the object 'o' throughout the list.
(define (list-intersperse ls o)
  (cond
    [(empty? (rest ls)) ls]
    [else
     (cons (first ls)
           (cons o 
                 (list-intersperse (rest ls) o)))]))

;; CONTRACT :: any -> string
;; Converts any object to a string.
;; Potentially in an ugly way.
(define (->string o)
  (format "~a" o))

(define (symbol<? a b)
  (string<? (symbol->string a)
            (symbol->string b)))
 
(define (snoc ls o)
  (reverse (cons o (reverse ls))))

(define debug
  (λ (str)
    (fprintf (current-error-port) str)))
