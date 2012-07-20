#lang racket

(require (file "util.rkt")
         (file "store.rkt")
         setup/path-to-relative)
(provide (all-defined-out))

;; Each library function is prefixed by the module it came from.
;; (require racket/runtime-path)
;; (define-runtime-path HERE ".")
(define cached-umbrella 'NoUmbrella)
(define (UMBRELLA)
  (when (symbol? cached-umbrella)
    (let ([v (simplify-path
              (if (windows?)
                  (build-path 
                   (find-system-path 'run-file) 'up)
                  (build-path 
                   (find-system-path 'run-file) 'up 'up))
                  )])
      (set! cached-umbrella v)))
  cached-umbrella)

(define (fix-separators o)
  (if (windows?)
      ;;(regexp-replace* "/" (format "~a" o) "\\\\")
      (let ([split (regexp-split "/" (format "~a" o))])
        (->string (apply build-path split)))
      (format "~a" o)))

(define path->relative
  (make-path->relative-string
   (list 
    (cons (λ () (format "~a" (occam-path)))
               "")
    (cons (λ () (format "~a" (UMBRELLA)))
               "")
    )))
  
(define (qs str)
  (case (system-type)
    [(windows) (format "\"~a\"" str)]
    [(unix macosx) str]))
    
(define (wsup p)
  (if (and (equal? (system-type) 'windows)
           (get-data 'server-running))
      (build-path p 'up)
      p))

;; build-bin-path :: string -> path
;; Takes the name of an executable and returns
;; a full path to it in the appropriate system's
;; bin directory.
(define (build-bin-path program)
  (qs
   (build-path (UMBRELLA) "bin" (format "~a" (system-type)) (format "~a" program))))

(define (config-file-path)
  (build-path (UMBRELLA) "config"))

(define (config-file platform)
  (let ([path 
         (build-path (config-file-path) (format "~a.rkt" platform))])
    (debug (format "CONFIG FILE: ~a~n" path))
    path))

(define (occam-path)
  (build-path (UMBRELLA) "occam" "flow"))

(define (firmware-path)
  (build-path (UMBRELLA) "occam" "firmware"))

;; occam-lib-path :: string -> path
;; Builds a path to an occam library
(define (occam-lib-path lib)
  (qs (build-path (UMBRELLA) "occam" "lib" (format "~a.lib" lib))))

(define temp-file-base "FLOW")
(define (build-temp-file extension)
  (build-path
      (UMBRELLA)
      "temp"
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

(define (json-file) 
  (debug "JSON FILE:~n")
  (debug (format "~a~n" (build-temp-file 'json)))
  (build-temp-file 'json))
(define (occ-file) (build-temp-file 'occ))
(define (tce-file) (build-temp-file 'tce))
(define (tbc-file) (build-temp-file 'tbc))
(define (hex-file) (build-temp-file 'hex))
(define (app-log) (build-path (UMBRELLA) (format "~a.log" temp-file-base)))

(define (avrdude-conf-file)
  (qs (build-path (config-file-path) "avrdude.conf")))

(define (temp-path)
  (build-path (UMBRELLA) "temp"))

(define (show-paths)
  (for-each
   (λ (s)
     (debug (format "~a~n" s)))
   (list 
    (UMBRELLA)
    (occam-lib-path 'forall)
    (build-temp-file 'temp)
    (isearch-list)
    (json-file)
    (occ-file)
    (tce-file)
    (tbc-file)
    (hex-file)
    (hex-file)
    (app-log)
    (temp-path)
    (avrdude-conf-file)
    (config-file-path)
    (config-file 'server)
    (config-file 'arduino)
    )))

(define (show-relative-paths)
  (for-each
   (λ (s)
     (debug (path->relative (format "~a~n" s))))
   (list 
    (UMBRELLA)
    (occam-lib-path 'forall)
    (build-temp-file 'temp)
    (isearch-list)
    (json-file)
    (occ-file)
    (tce-file)
    (tbc-file)
    (hex-file)
    (hex-file)
    (app-log)
    (temp-path)
    (avrdude-conf-file)
    (config-file-path)
    (config-file 'server)
    (config-file 'arduino)
    )))