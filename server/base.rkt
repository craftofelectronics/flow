#lang racket

(require (file "store.rkt"))
(provide (all-defined-out))

(define PORT "8000")
(define BASE-URL (format "http://localhost:~a" PORT))

(define KEY:ARDUINO "arduino")

