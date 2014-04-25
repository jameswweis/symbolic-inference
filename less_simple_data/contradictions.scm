(define contradictions
  (list

   ;; Contradiction 1
   (cons '(CAUSE (? a) (? b))
         '(CAUSE (? b) (? a)))

   ;; Contradiction 4
   (cons '(DOWNREGULATE (? x) (? y))
         '(UPREGULATE (? x) (? y)))

))

(pp contradictions)
(pp "contradictions done")
