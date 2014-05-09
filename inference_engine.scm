;;;; inference_engine.scm

(load "load")
(load "pattern_matcher")

(define all-knowledge)
(define all-rules)
(define all-compound_obj_aliases)

(define (ie:init)
  (set! all-knowledge '())
  (set! all-rules '())
  (set! all-compound_obj_aliases '()))

(define (ie:add-knowledge knowledge)
  (set! all-knowledge (append all-knowledge knowledge)))
  
(define (ie:add-aliases new-aliases)
  (set! all-compound_obj_aliases (append all-compound_obj_aliases new-aliases)))

(define (ie:add-rules new-rules)
  (set! all-rules (append all-rules new-rules)))
  
(define (ie:print-knowledge)
  (pp all-knowledge))

(define (ie:is-true statement context_predicate)
  (ie:infer context_predicate)

  (let ((matches_to_statement (ie:member statement all-knowledge)))
    (if (null? matches_to_statement)
      (displayln "FALSE.")
      (if (= 1 (length matches_to_statement))
        (impressive-print matches_to_statement)
        (begin
          (displayln "TRUE. Multiple matches found:")
          (pp matches_to_statement))))))

(define (ie:infer context_predicate)
  (define (on_match knowledge matched_statements new_statement)
    ;(pp (list "!!!!! on_match" matched_statements "=>" new_statement))
    (let* ((new_type (car new_statement))
           (new_args (cdr new_statement))
           (new_knowledge    
              (list new_type new_args (list
              (cons "inferred_from" matched_statements)))))
      (ie:add-knowledge-in-place new_knowledge)))
    
  (pm:match all-knowledge all-rules on_match all-compound_obj_aliases))
  
;;; ie:infer helpers

(define (ie:has-statement knowledge new_knowledge)
  (define (statement-of k)
    (cons (car k) (cadr k)))

  (if (null? knowledge) #f
      (or (equal? (statement-of new_knowledge) (statement-of (car knowledge)))
          (ie:has-statement (cdr knowledge) new_knowledge))))

(define (ie:add-knowledge-in-place new-knowledge)
  (if (not (ie:has-statement all-knowledge new-knowledge))
      (append-to-end! all-knowledge new-knowledge)))

;;; ie:is-true helpers

(define (impressive-print matches)
  (displayln "TRUE")
  (if (equal? "inferred_from" (car (car (cadr (cdr (car matches))))))
    (ip:inferred (cdr (car (cadr (cdr (car matches))))))
    (ip:original (cdr (car matches)))))

(define (ip:inferred statements)
  (display "Your statement is correct and was inferred from the following ")
  (display (/ (length statements) 3))
  (displayln " statements:")
  (pp statements))

(define (ip:original statement)
  (displayln "Your statement is correct and was inferred from the following statement:")
  (pp statement))

(define (ie:member statement current-knowledge)
  (ie:member-helper statement current-knowledge '()))

(define (ie:eq-arg? sArg kArg)
  (if (not (list? all-compound_obj_aliases))
      (equal? sArg kArg)
      (cond ((string? kArg) (equal? kArg sArg))
            ((symbol? kArg)
              (let ((alias_list (assoc sArg all-compound_obj_aliases)))
                (or (equal? kArg sArg)
                    (and alias_list (memq? kArg (cdr alias_list)) ))))
            (else (equal? kArg sArg)))))

(define (ie:eq-args? args1 args2)
  (cond ((and (null? args1) (null? args2)) #t)
        ((not (ie:eq-arg? (car args1) (car args2))) #f)
        (else (ie:eq-args? (cdr args1) (cdr args2)))))

(define (ie:eq-statement-knowledge? sType sArgs kType kArgs)
  (and (equal? sType kType) (equal? (length sArgs) (length kArgs)) (ie:eq-args? sArgs kArgs)))

(define (ie:member-helper statement current-knowledge matches)
  (if (null? current-knowledge) matches 
    (let*  ((statementType (car statement)) 
            (statementArgs (car (cdr statement))) 
            (knowledgeType (car (car current-knowledge))) 
            (knowledgeArgs (car (cdr (car current-knowledge))))
            (sk-equal (ie:eq-statement-knowledge? statementType statementArgs knowledgeType knowledgeArgs)))
      (if sk-equal
        (set! matches (cons (car current-knowledge) matches)))
      (ie:member-helper statement (cdr current-knowledge) matches))))