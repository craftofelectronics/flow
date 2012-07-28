#lang racket
(provide check-for-updates)

(require (file "paths.rkt")
         (file "params.rkt")
         (file "util.rkt")
         (file "store.rkt"))

(require net/url)

(define (check-for-updates)
  (with-handlers ([exn:fail? (λ (e)
                               ;; Die (mostly) silently.
                               (debug "check-for-updates FAILED.")
                               (debug (format "~n~a~n" e))
                               )])
    (check-for-updates-wrapped)
    
    ))

(define (check-for-updates-wrapped)
  (define params-local (read-params 'server))
  (define params-remote
    (call/input-url
     (string->url 
      (format "~a/~a" (get-data 'remote-url) "config/server.rkt"))
     get-pure-port 
     (λ (ip) 
       (read ip))))
  
  (define versions-local
    (hash-ref params-local 'versions))
  (define versions-remote
    (hash-ref params-remote 'versions))
  
  (define any-changed? false)
  
  (define to-update '())
  (define (push v)
    (set! to-update (cons v to-update)))
  
  ;; Need to handle hashes instead of lists...
  ;; Assume the remote is bigger than the local.
  ;; That is, an update was pushed to the remote.
  (hash-for-each 
   versions-remote
   (λ (k v)
     (let ([localv (hash-ref 
                    versions-local
                    k (λ () false))])
       (when (< localv v)
         (debug (format "~a < ~a~n" localv v))
         (push k)))))
  
  (when (not (empty? to-update))
    (fetch-updated-params "server.rkt" "config")
    (for-each (λ (v)
                (debug (format "Updating [~a]~n" v))
                (fetch-updated-params 
                 v (hash-ref 
                    (hash-ref params-remote
                              'update-paths) v)
                                      ))
              to-update))
  )

(define (strip-rkt str)
  (regexp-replace ".rkt" str ""))

(define (fetch-updated-params cfg-file cfg-path)
  (case (->sym cfg-path)
    [(config)
     (define new-file
       (call/input-url
        (string->url (format "~a/~a/~a"
                             (get-data 'remote-url) 
                             cfg-path
                             cfg-file))
        get-pure-port (λ (ip) (port->string ip))))
     (debug (format "Writing ~a~n" cfg-file))
     ;(debug new-file)
     (call-with-output-file
         (config-file (strip-rkt cfg-file))
       (λ (op) (fprintf op "~a" new-file))
       #:exists 'replace)]
    [(occam/flow) 
     (define new-file
       (call/input-url
        (string->url (format "~a/~a/~a"
                             (get-data 'remote-url) 
                             cfg-path
                             cfg-file))
        get-pure-port (λ (ip) (port->string ip))))
     (debug (format "Writing ~a~n" cfg-file))
     (call-with-output-file
         (build-path (occam-path) cfg-file)
       (λ (op) (fprintf op "~a" new-file))
       #:exists 'replace)]
    [(interface/blocks) 
     (define new-file
       (call/input-url
        (string->url (format "~a/~a/~a"
                             (get-data 'remote-url) 
                             cfg-path
                             cfg-file))
        get-pure-port (λ (ip) (port->string ip))))
     (debug (format "Writing ~a~n" cfg-file))
     (call-with-output-file
         (build-path (blocks-path) cfg-file)
       (λ (op) (fprintf op "~a" new-file))
       #:exists 'replace)]
    ))

