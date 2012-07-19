#lang racket

(provide binary-to-ihex)

; line_len = 16
(define line-length 16)

; # Record types in output file.
; DATA_RECORD = 0x00
; END_OF_FILE = 0x01
(define DATA-RECORD #x00)
(define END-OF-FILE #x01)

; f = open(sys.argv[2], "rb")
; data = f.read()
; f.close()

;; No tencstrip laying around...

(require (prefix-in fx: racket/fixnum)
         mzlib/string)
(define (write-line lob op)
  ;	# Compute checksum: twos-complement sum of all fields (except the
  ;	# checksum).
  ;	sum = 0
  (define sum 0)
  
  ; for b in bytes:
  ; sum += b
  (for-each (λ (b)
              (set! sum (fx:fx+ sum b)))
            lob)
  
  ;bytes += [(0x100 - (sum & 0xFF)) & 0xFF]
  (set! lob 
        (append
         lob
         (list (fx:fxand (- #x100 (fx:fxand sum #xFF)) #xFF))))
  
  ; f.write(":" + "".join(["%02x" % b for b in bytes]).upper() + "\r\n")
  (fprintf op ":")
  (for-each (λ (b)
              (let ([s (format "~X" b)])
                (string-uppercase! s)
                (when (< b 16)
                  (set! s (format "0~a" s)))
                (fprintf op s)))
            lob)
  (fprintf op "\r\n")
  )

(require rnrs/arithmetic/fixnums-6)
(define (loop ip op addr)
  ; while len(data) > 0:
  ; line_data = data[:line_len]
  ; data = data[line_len:]
  (let ([next-line (read-bytes line-length ip)])
    (unless (eof-object? next-line)
      ; bytes = [len(line_data), addr >> 8, addr & 0xFF, DATA_RECORD]
      (define bytes (append
                     (list 
                      (length (bytes->list next-line))
                      (fxarithmetic-shift-right addr 8)
                      (fx:fxand addr #xFF)
                      DATA-RECORD)
                     (bytes->list next-line)))
      ; bytes += map(ord, line_data)
      ; write_line(bytes)
      (write-line bytes op) 
      ; addr += len(line_data)
      (unless (zero? (bytes-length next-line))
        (loop ip op (+ addr (bytes-length next-line)))))))



(define (binary-to-ihex start-addr infile outfile)
  (define ip (open-input-file infile))
  (define op (open-output-file outfile))
  (loop ip op start-addr)
  (write-line (list #x00 #x00 #x00 END-OF-FILE) op)
  (close-input-port ip)
  (close-output-port op)
  )