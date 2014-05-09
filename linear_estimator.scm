;;;; linear_estimator.scm

(define (le:knowledge-from filename context)
  (let* ((f (open-input-file filename))
         (y (read f))
         (x (read f))
         (rows (le:rows f '()))  ; (y x pairs)
         (nrows (length rows))
        
         ; calculate cov
         (E_xy (/ (reduce-left + 0 (map (lambda(n) (* (car n) (cdr n))) rows)) nrows))
         (E_x (/ (reduce-left + 0 (map (lambda(n) (cdr n)) rows)) nrows))
         (E_y (/ (reduce-left + 0 (map (lambda(n) (car n)) rows)) nrows))
         (Cov_xy (- E_xy (* E_x E_y)))
        
         ; calculate var
         (E_y2 (/ (reduce-left + 0 (map (lambda(n) (expt (car n) 2)) rows)) nrows))
         (Var_y (- E_y2 (expt E_y 2)))
        
         ; correlation (y x cov(xy)/var(y))
         (corr (/ Cov_xy Var_y)))
    ;(pp corr)
    (le:generate-knowledge (le:relation corr) y x context)))

(define (le:rows file accumulated)
  (let ((v1 (read file)))
    (if (eof-object? v1)
      accumulated
      (let* ((n1 v1)
             (n2 (read file)))
        (le:rows file (cons (cons n1 n2) accumulated))))))

(define (le:relation correlation)
  (cond ((> correlation 0) 'CAUSE)
        ((< correlation 0) 'BLOCK)
        (else #f)))

(define (le:generate-knowledge relation arg1 arg2 context)
  (list relation (list arg1 arg2) context))