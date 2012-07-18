#lang racket/gui
(require (file "dispatch.rkt")
         (file "arduino.rkt")
         (file "params.rkt")
         (file "util.rkt")
         (file "store.rkt")
         (file "system.rkt")
         (file "updates.rkt"))

(define MIN-WIDTH 250)

(define f (new frame% [label "Flow"]))

(define ports (map ->string (list-arduinos-raw)))

(define bar
  (new menu-bar%
       [parent f]))

(define setup-menu
  (new menu%
       [parent bar]
       [label "Setup"]))

#|
(call-with-input-file 
                  (format "\\\\.\\COM~a" v)
                (λ (p)
                  (printf "~a~n" v))))
|#

(define (detect-step-one)
  (let ([d (new dialog% 
                [label "Unplug your Arduino"]
                [style (list 'close-button)]
                )])
    (new message%
         [parent d]
         [label "If your Arduino is plugged in, unplug it now."])
    (new button%
         [parent d]
         [label "Continue"]
         [callback (λ (b e) (send d show false))])
    (send d show true)))

(define (detect-step-two)
  (let ([d (new dialog% 
                [label "Now, plug it back in."]
                [style (list 'close-button)]
                )])
    (new message%
         [parent d]
         [label "Please plug your Arduino in again."])
    (new button%
         [parent d]
         [label "Continue"]
         [callback (λ (b e) (send d show false))])
    (send d show true)))

(define (detect-step-three)
  (let ([d (new dialog% 
                [label "I think we've got it."]
                [style (list 'close-button)]
                )])
    (new message%
         [parent d]
         [label 
          (format "My best guess:")])
    
    (if (empty? ports)
        (new message% [parent d] [label "I'm baffled."])
        (for-each (λ (p) 
                    (new message% 
                         [parent d]
                         [label (format "~a" p)]))
                  ports))
    (new button%
         [parent d]
         [label "OK"]
         [callback (λ (b e) (send d show false))])
    (send d show true)))

(define (sub ls1 ls2)
  (cond
    [(empty? ls1) '()]
    [(member (first ls1) ls2)
     (sub (rest ls1) ls2)]
    [else
     (cons (first ls1) (sub (rest ls1) ls2))
     ]))

(define (uniq ls1 ls2)
  (append (sub ls1 ls2) (sub ls2 ls1)))

(define find-arduino
  (new menu-item%
       [parent setup-menu]
       [label "Find my Arduino"]
       [callback 
        (λ (m e)
          (detect-step-one)
          (let ([before (list-arduinos-raw)])
            (detect-step-two)
            (let ([after (list-arduinos-raw)])
              (set! ports (uniq after before))
              (debug (format "B ~a A ~a U ~a~n" 
                             before after ports))
              (detect-step-three)
              (send port clear)
              (for-each (λ (p)
                          (send port append (->string p)))
                        ports)
              (send port refresh)
              (send port set-selection 0)
              )))]))

(define messages
  (λ (p)
    (λ ls
      (for-each (λ (i)
                  (new message%
                       [parent p]
                       [label i]))
                ls))))
  
(define setup-arduino
  (new menu-item%
       [parent setup-menu]
       [label "Setup my Arduino"]
       [callback 
        (λ (m e)
          (do-setup)
          (install-firmware)
          (let ([d (new dialog% 
                        [label "Go with the Flow"]
                        [style (list 'close-button)]
                        )])
            
            ((messages d) 
             "Arduino Status: Magical."
             " "
             "(See concurrency.cc for details.)")
            
            (new button%
                 [parent d]
                 [label "Awesome"]
                 [callback (λ (b e) (send d show false))])
            (send d show true)))]
       ))


(define port
  (new choice%
       [parent f]
       [label "Port"]
       [choices ports]
       [min-width MIN-WIDTH]
       [callback (λ (ch evt) 'DoNothingHere) ]))

(define types 
  '(("Arduino Uno" arduinouno)
    ("Arduino Duemillanove" arduino)))

(define type
  (new choice%
       [parent f]
       [label "Type"]
       [min-width MIN-WIDTH]
       [choices (map first types)]
       [callback (λ (ch evt) 'DoNothingHere)]
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

#|
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
|#


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
