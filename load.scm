;;; load.scm

; load lib/
(cd "lib")
(load "load")
(cd "..")

; Utilities

(define call/cc call-with-current-continuation)

(define (displayln x)
  (display x)(newline))

; for-each on a single list with reversed argument order
(define (for-each2 lst procedure)
  (for-each procedure lst))

; append obj to lst in place (mutates list)
(define (append-to-end! lst obj)
  (set-cdr! (last-pair lst) (list obj)))