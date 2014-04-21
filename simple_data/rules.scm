(define rules (list
  (cons (list '(CAUSE (? a) (? b))
              '(CAUSE (? b) (? c))  )
        '(CAUSE (? a) (? c)))
))

(pp rules)
(pp "rules done")