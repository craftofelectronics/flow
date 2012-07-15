#lang racket/gui
(require (file "dispatch.rkt")
         (file "arduino.rkt")
         (file "params.rkt")
         (file "util.rkt")
         (file "store.rkt"))

(define f (new frame% [label "Flow"]))

(define ports (map ->string (list-arduinos-raw)))
(define port
  (new choice%
       [parent f]
       [label "Port"]
       [choices ports]
       [callback (λ (ch evt)
                   (set-data!
                    'port
                    (list-ref ports (send port get-selection))))
                 ]))

(define types 
  '(("Arduino Uno" arduinouno)
    ("Old-Skool Arduino" arduino)))

(define type
  (new choice%
       [parent f]
       [label "Type"]
       [choices (map first types)]
       [callback (λ (ch evt)
                   (let ([ndx (send type get-selection)])
                     (read-params (list-ref types ndx))))] 
       ))

(define server-thread-id 'NoThreadID)
(define button
  (new button%
       [parent f]
       [label "Launch"]
       (callback (λ (b evt)
                   (when (symbol? server-thread-id)
                     ;; Read the parameters for the type...
                     (let ([ndx (send type get-selection)])
                       (read-params (list-ref (map second types) ndx)))
                     ;; Set the port
                     (set-data!
                      'port
                      (list-ref ports (send port get-selection)))
                     
                     (debug "SET PARAMETERS~n")
                     (show-params)
                     (debug "DONE SET PARAMETERS~n")
                     
                     ;; Make it so you can't do this again.
                     (send button set-label "Running")
                     
                     ;; Now launch the server
                     (set! server-thread-id 
                           (thread (λ () (serve))))
                     )))))


(send f show true)
