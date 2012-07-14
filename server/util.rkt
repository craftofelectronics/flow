#lang racket/base

(provide (all-defined))

;; CONTRACT :: (list-of any) any -> (list-of any)
(define (list-intersperse ls o)
  (cond
    [(empty? (rest ls)) ls]
    [else
     (cons (first ls)
           (cons o 
                 (list-intersperse (rest ls) o)))]))

(define (->string o)
  (format "~a" o))