#lang racket
(provide serve)

;; Server modules
;; Functionality for the server is broken down into modules
;; handling one or more endpoints in the dispatcher.
(require (file "util.rkt")
         (file "arduino.rkt")
         (file "store.rkt")
         (file "system.rkt")
         (file "paths.rkt")
         (file "params.rkt")
         (file "debug-state.rkt")
         web-server/dispatch 
         web-server/http
         web-server/servlet-env
         )

; FIXME
;; This should all be read from a conf file.
(define (set-platform-parameters req platform)
  (case (string->symbol platform)
    [(uno arduinouno)
     (set-data! 'platform 'uno)
     (set-data! 'baud 115200)
     (set-data! 'mcpu 'm328p)
     ]
    [else ; arduino
     (set-data! 'platform 'arduino)
     (set-data! 'baud 57600)
     (set-data! 'mcpu 'm328p)]
    )
  (response/xexpr 
   `(p ,(format "Set parameters at time ~a" (current-seconds))))
  )



(define (log-and-run req http-param)
  ;(define log-op (open-output-file (app-log) #:exists 'append))
  ;(fprintf log-op "~a~n" (current-seconds))
  ;(fprintf log-op "~n~a~n" (request-post-data/raw req))
  ;(close-output-port log-op)
  (run req (request-post-data/raw req))
  
  (response/xexpr 
   `(p ,(format "~a" (current-seconds)))))

(define-values (dispatch blog-url)
  (dispatch-rules
   [("run" (string-arg)) log-and-run]
   [("list") list-arduinos]
   [("set" (string-arg) (string-arg)) set-data/api]
   [("get" (string-arg)) get-data/api]   
   [("platform" (string-arg)) read-params/resp]
   [("blockfile" (string-arg)) get-block-set]
   ))

(define (get-block-set req ignore)
  (let ([bsf (get-data 'block-set-file)])
    (debug (format "BSF: ~a~n" bsf))
    (response/xexpr
     `(blocks ,bsf))))

(define (serve)
  (debug (format "Serving 'Flow for Arduino' up from [ ~a ]~n" (www-path)))
  (debug (format "~n===PATHS===~n"))
  (show-paths)
  ;;(load-params 'server)
  (debug "~nPARAMS~n")
  (set-data! 'server-running true)
  (show-params)
  
  (with-handlers ([exn:fail? 
                   (lambda (e) 'ServeFail)])
    (serve/servlet dispatch
                   #:launch-browser? #t
                   #:extra-files-paths (www-path)
                   #:server-root-path (UMBRELLA)
                   #:servlet-path "/"
                   #:servlet-regexp #rx"")))