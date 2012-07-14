#lang racket

(provide UMBRELLA
         temp-file-base
         json-file
         occ-file
         tce-file
         tbc-file
         hex-file
         app-log
         build-bin-path
         SEP
         conf-file)

;; Each library function is prefixed by the module it came from.
(require racket/runtime-path)
;; (define-runtime-path HERE ".")
(define (UMBRELLA) 
  (simplify-path
   (build-path 
    (find-system-path 'run-file) 'up 'up)))

(define temp-file-base "ARDU")

(define (build-bin-path program)
  (build-path (UMBRELLA) "bin" (format "~a" (system-type)) program))

(define (json-file) 
  (build-path (UMBRELLA)
              (format "~a.json" temp-file-base)))


(define (isearch-list)
  (map (λ (p)
         (build-path (UMBRELLA) p))
       (list 
        (build-path "occam" "flow")
        (build-path "occam" "lib")
        (build-path "occam" "include")
        (build-path "occam" "include" "arch" "m328p")
        (build-path "occam" "include" "arch" "common")
        (build-path "occam" "include" "platforms" "arduino"))))

(define (occ-file) 
  (build-path (UMBRELLA)
              (format "~a.occ" temp-file-base)))
(define (tce-file) 
  (build-path (UMBRELLA)
              (format "~a.tce" temp-file-base)))
(define (tbc-file) 
  (build-path (UMBRELLA)
              (format "~a.tbc" temp-file-base)))
(define (hex-file) 
  (build-path (UMBRELLA)
              (format "~a.hex" temp-file-base)))

(define (app-log)
  (build-path (UMBRELLA) 
              (format "~a.log" temp-file-base)))



(define (conf-file)
  (build-path (UMBRELLA) 
              "tvm" "common" "conf" "avrdude.conf"))

(define (SEP)
  (case (system-type)
    [(macosx unix) "/"]
    [else "\\"]))
 