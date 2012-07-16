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
  
  (define any-changed? false)
  
  (define to-update '())
  (define (push v)
    (set! to-update (cons v to-update)))
  
  (for-each (λ (vl vr)
              (debug (format "Comparing local[~a] to remote[~a]~n" (third vl) (third vr)))
              (when (< (third vl) (third vr))
                (debug (format "~a < ~a~n" (third vl) (third vr)))
                (push vl)))
            versions-local
            versions-remote)
  
  (when (not (empty? to-update))
    (fetch-updated-params (list "server.rkt" "config" 'NotNeededForFetch))
    (for-each (λ (v)
                (debug (format "Updating [~a]~n" v))
                (fetch-updated-params v))
              to-update))
  )

(define (strip-rkt str)
  (regexp-replace ".rkt" str ""))

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
     (debug (format "Writing ~a~n" (first remote)))
     ;(debug new-file)
     (call-with-output-file
         (config-file (strip-rkt (first remote)))
       (λ (op) (fprintf op "~a" new-file))
       #:exists 'replace)
     ]
    
    [(occam/flow) 
     (define new-file
       (call/input-url
        (string->url (format "~a/~a/~a"
                             (get-data 'remote-url) 
                             (second remote)
                             (first remote)))
        get-pure-port (λ (ip) (port->string ip))))
     (debug (format "Writing ~a~n" (first remote)))
     (call-with-output-file
         (build-path (occam-path) (first remote))
       (λ (op) (fprintf op "~a" new-file))
       #:exists 'replace)]
    ))

