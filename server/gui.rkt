#lang racket/gui
(require (file "dispatch.rkt"))

(define server-thread-id 'NoThreadID)

(define f (new frame% [label "Flow"]))
(define button
  (new button%
       [parent f]
       [label "Launch"]
       (callback (λ (b evt)
                   (when (not (number? server-thread-id))
                     (set! server-thread-id 
                           (thread (λ () (serve)))))))))

(send f show true)
       