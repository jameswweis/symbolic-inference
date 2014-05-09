; load lib/

(cd "lib")
(load "load")
(cd "..")

; Utilities

(define call/cc call-with-current-continuation)

; for-each on a single list with reversed argument order
(define (for-each2 lst procedure)
  (for-each procedure lst))
  
(define (append-to-end! lst obj)
  (set-cdr! (last-pair lst) (list obj)))