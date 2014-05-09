(define rules
  (list
   ;; Rule 1: (a -> b) + (b -> c) -> (a -> c)
   (cons
    (list '(CAUSE (? a) (? b))
          '(CAUSE (? b) (? c)))
    '(CAUSE (? a) (? c)))

   ;; Rule 2: (a -| b) + (b -| c) -> (a -> c)
   (cons
    (list '(BLOCK (? a) (? b))
          '(BLOCK (? b) (? c)))
    '(CAUSE (? a) (? c)))

   ;; Rule 3: (a -> b) + (b -| c) -> (a -| c)
   (cons
    (list '(CAUSE (? a) (? b))
          '(BLOCK (? b) (? c)))
    '(BLOCK (? a) (? c)))

   ;; Rule 4: (a -| b) + (b -> c) -> (a -| c)
   (cons
    (list '(BLOCK (? a) (? b))
          '(CAUSE (? b) (? c)))
    '(BLOCK (? a) (? c)))

   ;; Rule 5: (a -| b) + (b -> c) -> (a -| c)
   (cons
    (list '(BLOCK (? a) (? b))
          '(CAUSE (? b) (? c)))
    '(BLOCK (? a) (? c)))

   ;; Rule 6: (a + b -> c) + (c -> d) -> (a + b -> d)
   (cons
    (list '(CAUSE (? a) (? b) (? c))
          '(CAUSE (? c) (? d)))
    '(CAUSE (? a) (? b) (? d)))

   ;; Rule 7: (a + b -> c) + (c -| d) -> (a + b -| d)
   (cons
    (list '(CAUSE (? a) (? b) (? c))
          '(BLOCK (? c) (? d)))
    '(BLOCK (? a) (? b) (? d)))

   ;; Rule 8: (a + b -> c) + (c -| d) -> (a + b -| d)
   (cons
    (list '(CAUSE (? a) (? b) (? c))
          '(BLOCK (? c) (? d)))
    '(BLOCK (? a) (? b) (? d)))

   ;; Rule 9: (a -> b) + (c -> d) + (b + d -> e) -> (a + c -> e)
   (cons
    (list '(CAUSE (? a) (? b))
          '(CAUSE (? c) (? d))
          '(CAUSE (? b) (? d) (? e)))
    '(BLOCK (? a) (? c) (? e)))

   ;; Rule 10: (a -| b) + (b + d -> e) -| (a -| e)
   (cons
    (list '(BLOCK (? a) (? b))
          '(CAUSE (? b) (? d) (? e)))
    '(BLOCK (? a) (? e)))

   ;; Rule 11: (a -| b) + (c -| d) + (b + d -| e) -> (a + c -> e)
   (cons
    (list '(BLOK (? a) (? b))
          '(BLOCK (? c) (? d))
          '(BLOCK (? b) (? d) (? e)))
    '(CAUSE (? a) (? c) (? e)))

   ))

(pp rules)
(pp "rules done")
