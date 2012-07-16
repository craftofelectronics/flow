#lang racket
(provide check-for-updates)

(require (file "paths.rkt")
         (file "params.rkt")
         (file "util.rkt")
         (file "store.rkt"))

(require net/url)

(define (check-for-updates)
  (define params-local (read-params 'server))
  (define params-remote
    (call/input-url
     (string->url 
      (format "~a/~a" (get-data 'remote-url) "config/server.rkt"))
     get-pure-port 
     (λ (ip) 
       (let ([str (port->string ip)])
         (printf "~a~n" str)
         (call-with-input-string
          str (λ (ip)
                (read ip)))))))
  
  (define versions-local
    (hash-ref params-local 'versions))
  (define versions-remote
    (hash-ref params-remote 'versions))
  
  (for-each (λ (vl vr)
              (debug (format "Comparing local[~a] to remote[~a]~n" (third vl) (third vr)))
              (when (> (third vr) (third vl))
                (fetch-updated-params vr)))
            versions-local
            versions-remote)
  )

(define (fetch-updated-params remote)
  (case (->sym (second remote))
    [(config)
     (define new-file
       (call/input-url
        (string->url (format "~a/~a/~a"
                             (get-data 'remote-url) 
                             (second remote)
                             (first remote)))
        get-pure-port (λ (ip) (port->string ip))))
     (debug (format "Fetching ~a~n" (first remote)))
     (debug new-file)]
  
    [(occam/flow) '...]))

  
  