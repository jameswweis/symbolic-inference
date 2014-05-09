;;;; linear_estimator.scm
#|
The linear estimator takes a table of molecule concentrations
(stored in a text file) and generates a knowledge statement.

The table must have 2 columns, one for each molecule.
The first row contains the molecule names; the other rows contain
their concentrations. For example:

  molA molB
  1 50
  2 40
  3 30
  4 20
  5 10

The linear estimator calculates the correlation of A and B and
outputs a knowledge statement for "A blocks B" or "A causes B"
depending on whether the correlation is positive or negative.

That knowledge can be fed directly to the inference engine.

Interface:
- (le:generate-knowledge relation arg1 arg2 context)

|#

; Returns a knowledge statement generated from the table stored in
; the specified file. The knowledge statement is fed the specified
; context
(define (le:knowledge-from filename context)
  ; y = first molecule
  ; x = second molecule
  ; correlation is the m in 'x = my + b' linear estimator
  (let* ((f (open-input-file filename))
         (y (read f))
         (x (read f))
         (rows (le:rows f '()))  ; (y x pairs)
         (nrows (length rows))
        
         ; calculate cov(Y,X) = E(XY) - E(X)E(Y)
         (E_xy (/ (reduce-left + 0 (map (lambda(n) (* (car n) (cdr n))) rows)) nrows))
         (E_x (/ (reduce-left + 0 (map (lambda(n) (cdr n)) rows)) nrows))
         (E_y (/ (reduce-left + 0 (map (lambda(n) (car n)) rows)) nrows))
         (Cov_xy (- E_xy (* E_x E_y)))
        
         ; calculate var(Y) = E(Y^2) - (E(X))^2
         (E_y2 (/ (reduce-left + 0 (map (lambda(n) (expt (car n) 2)) rows)) nrows))
         (Var_y (- E_y2 (expt E_y 2)))
        
         ; correlation = cov(X,Y)/var(Y)
         (corr (/ Cov_xy Var_y)))
    ;(pp corr)
    (le:generate-knowledge (le:relation corr) y x context)))

; Returns the rows of a file as a list of pairs
(define (le:rows file accumulated)
  (let ((v1 (read file)))
    (if (eof-object? v1)
      accumulated
      (let* ((n1 v1)
             (n2 (read file)))
        (le:rows file (cons (cons n1 n2) accumulated))))))

; Thresholding function for CAUSE and BLOCK
; Could change thresholds to be more strict
(define (le:relation correlation)
  (cond ((> correlation 0) 'CAUSE)
        ((< correlation 0) 'BLOCK)
        (else #f)))

; Returns a knowledge statement of the form:
; (relation (arg1 arg2) context)
(define (le:generate-knowledge relation arg1 arg2 context)
  (list relation (list arg1 arg2) context))