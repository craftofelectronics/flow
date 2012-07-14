#lang racket

(provide (all-defined-out))

;; Each library function is prefixed by the module it came from.
(require racket/runtime-path)
;; (define-runtime-path HERE ".")
(define (UMBRELLA) 
  (simplify-path
   (build-path 
    (find-system-path 'run-file) 'up 'up)))

;; build-bin-path :: string -> path
;; Takes the name of an executable and returns
;; a full path to it in the appropriate system's
;; bin directory.
(define (build-bin-path program)
  (build-path (UMBRELLA) "bin" (format "~a" (system-type)) program))

;; occam-lib-path :: string -> path
;; Builds a path to an occam library
(define (occam-lib-path lib)
  (build-path (UMBRELLA) "occam" "lib" (format "~a.lib" lib)))

(define temp-file-base "FLOW")
(define (build-temp-file extension)
  (build-path
   (find-system-path 'temp-dir)
   (format "~a.~a" 
           temp-file-base
           extension)))
 
;; Path to the www directory.
;; The webserver expects a list of directories.
(define (www-path)
  (list (build-path (UMBRELLA) "interface")))

(define (isearch-list)
  (map (λ (p)
         (build-path (UMBRELLA) p))
       (list 
        (build-path "occam" "flow")
        (build-path "occam" "lib")
        (build-path "occam" "include")
        (build-path "occam" "include" "arch" "common")
        ;; FIXME
        ;; These need to be based on the board being used.
        (build-path "occam" "include" "arch" "m328p")
        (build-path "occam" "include" "platforms" "arduino"))))

(define (json-file) (build-temp-file 'json))
(define (occ-file) (build-temp-file 'occ))
(define (tce-file) (build-temp-file 'tce))
(define (tbc-file) (build-temp-file 'tbc))
(define (hex-file) (build-temp-file 'hex))
(define (app-log) (build-path (UMBRELLA) (format "~a.log" temp-file-base)))

(define (avrdude-conf-file)
  (build-path (UMBRELLA) "conf" "avrdude.conf"))