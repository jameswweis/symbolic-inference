(define rules
  (list
   ;; Law 1
   (cons
    (list '(CAUSE (? a) (? b))
          '(CAUSE (? b) (? c)))
    '(CAUSE (? a) (? c)))

   ;; Law 2
   (cons
    (list '(UPREGULATE (? a) (? b))
          '(DOWNREGULATE (? b) (? c)))
    '(DOWNREGULATE (? a) (? c)))

   ;; Law 3
   (cons
    (list '(CAUSE (? a) (? b))
          '(DOWNREGULATE (? b) (? c)))
    '(DOWNREGULATE (? a) (? c)))

   ;; Law 4
   (cons
    (list '(UPREGULATE (? a) (? b))
          '(UPREGULATE (? b) (? c)))
    '(UPREGULATE (? a) (? c)))
))

(pp rules)
(pp "rules done")
