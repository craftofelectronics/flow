#lang racket/gui
(require (file "dispatch.rkt")
         (file "arduino.rkt")
         (file "params.rkt")
         (file "paths.rkt")
         (file "util.rkt")
         (file "store.rkt")
         (file "system.rkt")
         (file "updates.rkt"))

;; Need these early.
(load-params 'server)
(show-paths)
(show-relative-paths)
;; Load changes from remote.
(check-for-updates)
(load-params 'server)

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
          (define end 0)
          (define start (current-seconds))
          (do-setup)
          (install-firmware)
          (set! end (current-seconds))
          
          (let ([d (new dialog% 
                        [label "Go with the Flow"]
                        [style (list 'close-button)]
                        )])
            (if (< (- end start) 3)
                ((messages d) 
                 "Our suspicion:"
                 " "
                 "SOFTWARE FAIL"
                 " "
                 "Go to http://concurrency.cc/, click on 'Support',"
                 "and ask for help on the 'users' mailing list.")
                ((messages d) 
                 "Arduino Status: Magical."
                 " "
                 "(See concurrency.cc for details.)"))
            
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

;; This should move to config file.
(define types 
  '(("Arduino Uno" arduinouno)
    ("Arduino Duemillanove" arduino)
    ("Raspberry-Pi (TBA)" raspberry-pi)
    ))


(define type
  (new choice%
       [parent f]
       [label "Type"]
       [min-width MIN-WIDTH]
       [choices (map first types)]
       [callback (λ (ch evt) 'DoNothingHere)]
       ))


(define (get-block-set-file name)
  (let ([h (get-data 'blocksets)])
    (hash-ref (hash-ref h (->string name)) "filename")))

(define (set-block-set)
  (let* ([name (send block-set get-string (send block-set get-selection))]
         [block-set-file (get-block-set-file name)])
    (debug (format "Setting block set to [~a]~n" block-set-file))
    (set-data! 'block-set-file block-set-file))
  )

; Part of server.rkt
;(load-params 'blocksets)
(define (list-block-sets)
  (let ([h (get-data 'blocksets)]
        [keys '()])
    (hash-for-each
     h (λ (k v)
         (set! keys (cons (list k
                                (hash-ref (hash-ref h k) "sort"))
                                keys))))
    (map first (sort keys < #:key second))
    ))

(define block-set
  (new choice%
       [parent f]
       [label "Block Set"]
       [min-width MIN-WIDTH]
       [choices (list-block-sets)]
       [callback (λ (ch evt)
                   (set-block-set)
                   )]
       ))

(define hp
  (new horizontal-pane%
       [parent f]))

(define (do-setup)
  (let ([ndx (send type get-selection)])
    ;; Load parameter data for the board type
    (load-params (list-ref (map second types) ndx)))
  
  ;; Choose the left-hand side blocksets
  (set-block-set)
  
  ;; Set the port
  (set-data!
   'port
   (if (empty? ports)
     "Arduino Not Found."
     (list-ref ports (send port get-selection))))
  (show-params))

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


(send f show true)
