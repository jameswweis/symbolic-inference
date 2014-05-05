;;;; File: load.scm

; Pattern matcher:

(load "ghelper")
(load "matcher")

(define call/cc call-with-current-continuation)

; Utilities

; for-each on a single list with reversed argument order
(define (for-each2 lst procedure)
  (for-each procedure lst))