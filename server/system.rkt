#lang racket
(require racket/system
         (file "util.rkt")
         (file "base.rkt")
         (file "paths.rkt")
         (file "jsonconv.rkt")
         (file "store.rkt"))

(provide run)


;; RUNNING COMMANDS
;; We'll define commands as S-expressions. The language
;; looks like
;; (cmd -flag0 (-flag1 value1) (= -p1 value2))
;; which becomes
;; "cmd -flag0 -flag1 value1 -p1=value2"
;; Note we don't insert hyphens, but we make sure
;; spaces come out right.

(struct cmd (app args) #:transparent)
(struct arg2 (flag value) #:transparent)
(struct arg1 (flag) #:transparent)
(struct set (param value) #:transparent)

(define (parse sexp)
  (match sexp
    [`(= ,rand1 ,rand2)
     (set rand1 rand2)]
    [`(,command ,args ...)
     (cmd command (map parse args))]
    ;; FIXME: The list of length two is subsumed by
    ;; the previous rule... unnecessary?
    [`(,flag ,value)
     (arg2 flag value)]
    [flag/value
     (arg1 flag/value)]))


(define (render ast)
  (match ast
    [(struct cmd (command args))
     (format "~a ~a" 
             command
             (apply string-append
                    (list-intersperse (map render args) " ")))]
    [(struct set (param value))
     (format "~a=~a" param value)]
    [(struct arg2 (flag value))
     (format "~a ~a" flag value)]
    [(struct arg1 (flag/value))
     (format "~a" flag/value)]))

(define (system-call prog flags)
  (format "~a ~a"
          (build-bin-path prog)
          (render (parse flags))))

#;(define (exe cmd)
  (let-values ([(from-stdout
                 to-stdin
                 process-id
                 from-stderr
                 status-fun) (apply values (process cmd))])
    (let loop ([status (status-fun 'status)])
      (cond
        [(or (equal? status 'done-ok)
             (equal? status 'done-error))
         (close-input-port from-stdout)
         (close-input-port from-stderr)
         (close-output-port to-stdin)]
        [else (loop (status-fun 'status))])
      )))

(define (exe cmd)
  (system cmd))

(define (compile-cmd fname)
  (system-call
  'occ21
  `(-t2 -V -etc -w -y -znd -znec 
        -udo -zncc -init -xin -mobiles 
        -zrpe -zcxdiv -zcxrem -zep -b -tle 
        -DEF (= F.CPU 16000000) 
        -DEF OCCBUILD.TVM ,fname)))

(define (save-json-file json)
  (define op (open-output-file (json-file) #:exists 'replace))
  (fprintf op json)
  (newline op)
  (close-output-port op)
  (json-file))

(define (transform-json-file)
  (define op (open-output-file (occ-file) #:exists 'replace))
  (define xformed (json->occ (file->string (json-file))))
  (fprintf op "~a~n" xformed)
  (close-output-port op))

(define (report banner cmd)
  (printf "~n====~n~a~n====~n" banner)
  (printf "~a~n" cmd))

(define (compile-occam-file)
  (define isearch (apply string-append 
                         (list-intersperse 
                          (map ->string (isearch-list))
                          ":")))
  (define cmd (compile-cmd (occ-file)))
  
  ;(set! cmd (format "export ISEARCH=~a ; ~a" isearch cmd))
  
  (putenv "ISEARCH" isearch)
  (report "ISEARCH" (getenv "ISEARCH"))
  (current-directory (temp-path))
  (report 'COMPILE cmd)
  (exe cmd))

(define (plinker-cmd)
  (system-call
   'plinker.pl
   `(-s -o ,(tbc-file)
        ,(->string (occam-lib-path 'forall))
        ,(tce-file))))

(define (plink)
  (report 'PLINK (plinker-cmd))
  (exe (plinker-cmd)))

(define (bin2hex-cmd)
  (system-call
   'binary-to-ihex
   `(0x4F00 ,(tbc-file) ,(hex-file))))

(define (bin2hex)
  (report 'BINARY-TO-IHEX (bin2hex-cmd))
  (exe (bin2hex-cmd)))

(define-syntax (when-file stx)
  (syntax-case stx ()
    [(when-file file body ...)
     #`(when (file-exists? file)
         body ...)]))

(define (cleanup-temp-files)
  (for-each (λ (f)
              (when-file f (delete-file f)))
            (list (json-file)
                  (occ-file)
                  (tce-file)
                  (tbc-file)
                  (hex-file))))

(define (build-port sp)
  (define PORT
    (case (system-type)
      [(macosx unix) (format "/dev/~a" sp)]
      [(windows) "FIXME"]))
  PORT)

;; FIXME
;; There needs to be more parameterization here.
(define (avrdude-cmd sp)
  (system-call
   'avrdude
   `(-C ,(->string (avrdude-conf-file))
        -V -F 
        (-p ,(get-data 'mcpu))
        (-b ,(get-data 'baud))
        (-c arduino)
        (-P ,(build-port sp))
        -D -U 
        ,(format "flash:w:~a" (hex-file)))))

(define (avrdude)
  (define ARDUINO-PORT (get-data 'port))
  (define cmd (avrdude-cmd ARDUINO-PORT))
  (report 'AVRDUDE cmd)
  (when ARDUINO-PORT
    (exe cmd)))

(define (show-table)
  (for-each (λ (k)
              (printf "~a : ~a~n" k (get-data k)))
            (get-keys)))

(define (run req json)
  (show-table)
  (cleanup-temp-files)
  (save-json-file (format "~a" json))
  (transform-json-file)
  (when-file (occ-file)
    (compile-occam-file))
  (when-file (tce-file)
    (plink))
  (when-file (tbc-file)
    (bin2hex))
  
  (when-file (hex-file)
    (printf "Uploading~n")
    (avrdude))
  )


