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
  (case (system-type)
    [(macosx)
     (filter (λ (str)
               (and (regexp-match "tty" str)
                    (regexp-match "usb" str)))
             (directory-list "/dev"))]
    [(unix)
     (filter (λ (str)
               (or (regexp-match "USB[0-9]+" str)
                   (regexp-match "ACM[0-9]+" str)))
             (directory-list "/dev"))]
    [(win) (list "NO ARDUINO")]))

(define (list-arduinos req)
  (define devices (list-arduinos-raw))
  (define list-items
    (map (λ (dev) (make-set-url dev)) devices))
  (response/xexpr
   `(html
     (body
      (ul ,@list-items)))))