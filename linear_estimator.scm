(define (le:rows file)
  (list (cons 1 2) (cons 2 4)))

; returns (y x cov(xy)/var(y))
(define (le:knowledge-from filename context)
  (let* (
    (f (open-input-file filename))
    (y (read f))
    (x (read f))
    (rows (le:rows f))  ; (y x pairs)
    (nrows (length rows))
    
    ; calculate cov
    (E_xy (/ (reduce-left + 0 (map (lambda(n) (* (car n) (cdr n))) rows)) nrows))
    (E_x (/ (reduce-left + 0 (map (lambda(n) (cdr n)) rows)) nrows))
    (E_y (/ (reduce-left + 0 (map (lambda(n) (car n)) rows)) nrows))
    (Cov_xy (- E_xy (* E_x E_y)))
    
    ; calculate var
    (E_y2 (/ (reduce-left + 0 (map (lambda(n) (expt (car n) 2)) rows)) nrows))
    (Var_y (- E_y2 (expt E_y 2)))
    
    ; correlation
    (corr (/ Cov_xy Var_y))
  )
  (le:generate-knowledge (le:relation corr) y x context)))

(define (le:relation relation)
  (cond ((> 0) 'CAUSE)
        ((< 0) 'BLOCK)
        (else #f)))

(define (le:generate-knowledge relation arg1 arg2 context)
  (list relation (list arg1 arg2) context))

(pp (le:knowledge-from "table.txt" '()))