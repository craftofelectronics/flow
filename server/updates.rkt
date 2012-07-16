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
      (format "~a~a" (get-data 'config-url) "server.rkt"))
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
              (when (> (second vr) (second vl))
                (fetch-updated-params (first vr))))
            versions-local
            versions-remote)
  )

(define (fetch-updated-params file)
  (debug (format "Fetching ~a~n" file)))
  