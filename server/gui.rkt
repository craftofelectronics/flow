#lang racket/gui
(require (file "dispatch.rkt")
         (file "arduino.rkt")
         (file "params.rkt")
         (file "util.rkt")
         (file "store.rkt")
         (file "system.rkt")
         (file "updates.rkt"))

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

(define hp
  (new horizontal-pane%
       [parent f]))

(define (do-setup)
  (let ([ndx (send type get-selection)])
    (load-params (list-ref (map second types) ndx)))
  ;; Set the port
  (set-data!
   'port
   (list-ref ports (send port get-selection)))
  (show-params))

(define firmware
  (new button%
       [parent hp]
       [label "Setup"]
       [callback (λ (b e)
                   (send firmware set-label "Installing")
                   (do-setup)
                   (install-firmware)
                   (send firmware set-label "Installed")
                   )]))


(define server-thread-id 'NoThreadID)
(define launch
  (new button%
       [parent hp]
       [label "Launch"]
       (callback (λ (b evt)
                   (when (symbol? server-thread-id)
                     ;; Read the parameters for the type...
                     (do-setup)
                     ;; Make it so you can't do this again.
                     (send launch set-label "Running")
                     ;; Now launch the server
                     (set! server-thread-id 
                           (thread (λ () (serve))))
                     )))))

(define quit
  (new button%
       [parent hp]
       [label "Quit"]
       (callback (λ (b e)
                   (when (not (symbol? server-thread-id))
                     (kill-thread server-thread-id))
                   (exit)))))

(load-params 'server)
(check-for-updates)
(send f show true)
