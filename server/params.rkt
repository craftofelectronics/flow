#lang racket
(require (file "paths.rkt")
         (file "store.rkt")
         (file "util.rkt"))

(require web-server/http)

(provide (all-defined-out))

(define (read-params platform)
  (let ([hash (call-with-input-file
                  (config-file platform)
                (λ (inp) (read inp)))])
    hash
    ))

(define (load-params platform)
  (let ([hash (call-with-input-file
                  (config-file platform)
                (λ (inp) (read inp)))])

    (hash-for-each
     hash (λ (k v)
            (set-data! k v)))
    ))

(define (show-params)
  (for-each (λ (k)
              (debug (format "~a [~a]~n" k (get-data k))))
            (get-keys)))
  
(define (read-params/resp req platform)
  (read-params platform)
  (show-params)
  (response/xexpr 
   `(p ,(format "~a" (current-seconds)))))