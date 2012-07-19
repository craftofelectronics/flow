#lang racket
(require racket/system
         mzlib/file
         setup/path-to-relative
         (file "util.rkt")
         (file "base.rkt")
         (file "paths.rkt")
         (file "jsonconv.rkt")
         (file "store.rkt")
         (file "bin-to-ihex.rkt"))

(provide run install-firmware)


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
(struct nospace (flag value) #:transparent)

(define (parse sexp)
  (match sexp
    [`(= ,rand1 ,rand2)
     (set rand1 rand2)]
    [`(nospace ,flag ,val)
     (nospace flag val)]
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
     (format "~a" flag/value)]
    [(struct nospace (flag value))
     (format "~a~a" flag value)]
    ))

(define (system-call prog flags)
  (format "~a ~a"
          (build-bin-path prog)
          (render (parse flags))))

(define (exe cmd)
  (system cmd))

(define (compile-cmd fname)
  
  (report 'ISEARCH-LIST 
          (format "~a~n" (isearch-list)))
    
  (system-call
  'occ21
  `(-t2 -V -etc -w -y -znd -znec 
         -udo -zncc -init -xin -mobiles 
         -zrpe -zcxdiv -zcxrem -zep -b -tle 
         -DEF (= F.CPU 16000000) -DEF OCCBUILD.TVM
         ,(qs fname))))

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
  (debug (format "~n====~n~a~n====~n" banner))
  (debug (format "~a~n" cmd)))

(define (identity o) o)
(define (compile-occam-file)
  
  (report 'DEFINE-ISEARCH " ")
  (define isearch (apply string-append 
                         (list-intersperse 
                          (map ->string (isearch-list))
                          (if (equal? (system-type) 'windows)
                              ";"
                              ":"))))
  
  (report 'BUILD-COMPILE-CMD " ")
  (define cmd (compile-cmd (occ-file)))
  
 
  (putenv "ISEARCH" isearch)
  (report "ISEARCH" (getenv "ISEARCH"))
  
  (report 'CURRENT-DIRECTORY-BEFORE (format "~a" (current-directory)))
  (current-directory (temp-path))
  
  (report 'CURRENT-DIRECTORY-AFTER (format "~a" (current-directory)))
  
  (report 'COMPILE cmd)
  (exe cmd))

(define (plinker-cmd)
  ;; Need to add some DLLs to the path?
  (report 'PATH (getenv "PATH"))
  
  (when (windows?)
    (define (dll sym)
      (build-path (UMBRELLA) "bin" "windows" (->string sym)))
    (let ([PATH (getenv "PATH")])
      (define updated-path
        (apply string-append
               (list-intersperse
                (list 
                 (->string (build-path (UMBRELLA) "bin" "windows"))
                 (->string (build-path (UMBRELLA) "bin" "windows" "Microsoft.VC90.CRT"))
                 PATH)
                ";")))
      (putenv "PATH" updated-path)
      (report 'UPDATED-PATH (getenv "PATH"))
      ))
     
  (system-call
   (if (windows?) 'plinker.exe 'plinker.pl)
   `(-s -o ,(qs (tbc-file))
        ,(->string (occam-lib-path 'forall))
        ,(qs (tce-file)))))

(define (plink)
  (report 'PLINK (plinker-cmd))
  (exe (plinker-cmd)))

(define (bin2hex-cmd)
  (system-call
   (if (windows?) 'binary-to-ihex 'binary-to-ihex.exe)
   `(0x4F00 ,(qs (tbc-file)) 
            ,(qs (hex-file)))))

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
      [(windows) sp]))
  PORT)

;; FIXME

;; There needs to be more parameterization here.
#|
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
|#

(define (avrdude-cmd sp file)
  (report 'FILE (format "~a" file))
  (report 'FIXED (fix-separators file))
  
  (system-call
   'avrdude
   `(-C ,(->string (avrdude-conf-file))
        -V -F 
        (-p ,(get-data 'mcpu))
        (-b ,(get-data 'baud))
        (-c arduino)
        (-P ,(build-port sp))
        -D -U 
        ,(format "flash:w:~a" (->string file)))))

(define (avrdude)
  (define ARDUINO-PORT (get-data 'port))
  (define cmd (avrdude-cmd ARDUINO-PORT 
                           ;(fix-separators (path->relative (hex-file)))
                           (format "~a.hex" temp-file-base)
                           #;(hex-file)
                           ))
  (report 'AVRDUDE cmd)
  
  (when ARDUINO-PORT
    (exe cmd)))

(define (show-table)
  (for-each (λ (k)
              (printf "~a : ~a~n" k (get-data k)))
            (get-keys)))

(define (run req json)
  (report 'SHOW-TABLE " ")
  (show-table)
  
  (report 'CLEANUP-TEMP-FILES " ")
  (cleanup-temp-files)
  
  (report 'SAVE-JSON-FILE " ")
  (save-json-file (format "~a" json))
  
  (report 'TRANSFORM-JSON-FILE " ")
  (transform-json-file)
  
  (report 'WHEN-OCC-FILE " ")
  (when-file (occ-file)
             (report 'COMPILE-OCC-FILE " ")
             (compile-occam-file))
  
  (when-file (tce-file)
             (report 'PLINK " ")
             (plink))
  
  (when-file (tbc-file)
             (report 'BIN-TO-HEX " ")
             ;(bin2hex)
             (binary-to-ihex
              (get-data 'start-address)
              (tbc-file)
              (hex-file)))
  
  (when-file (hex-file)
             (report 'UPLOADING " ")
             (avrdude))
  )


(define (get-firmware-path platform)
  (build-path (firmware-path) (get-data 'firmware)))
     

(define (install-firmware)
  (let ([path (get-firmware-path (get-data 'platform))])
    (define ARDUINO-PORT (get-data 'port))
    ;; FIXME: This works on Windows.
    ;; It solves an absolute path problem that makes AVRDUDE choke.
    ;; Dunno if it should be on all platforms.
    (define cmd (avrdude-cmd ARDUINO-PORT (path->relative path)))
  
    (report 'FIRMWARE cmd)
    (exe cmd)))