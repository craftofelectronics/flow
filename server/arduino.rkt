#lang racket
(require (file "base.rkt"))
(require web-server/http
         (file "store.rkt")
         (file "paths.rkt")
         )

(provide list-arduinos
         list-arduinos-raw
         arduino?)

(define (arduino? req)
  (get-data 'arduino-port))
  

;; list-arduinos
(define (make-set-url dev)
  (define the-device (path->string dev))
  `(li (a ((href ,(format "~a/set/~a/~a" (get-data 'base-url)
                          'port the-device)))
          ,the-device)))

(define (list-arduinos-raw)
  (define devices
    (filter (λ (str)
              (and (regexp-match "tty" str)
                   (regexp-match "usb" str)))
            (directory-list "/dev")))
 devices)

(define (list-arduinos req)
  (define devices (list-arduinos-raw))
  (define list-items
    (map (λ (dev) (make-set-url dev)) devices))
  (response/xexpr
   `(html
     (body
      (ul ,@list-items)))))