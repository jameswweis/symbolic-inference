(cd "lib")
(load "load")
(cd "..")

; for-each on a single list with reversed argument order
(define (for-each2 lst procedure)
  (for-each procedure lst))

#|
//pm:match procedure
pm:match knowledge rules on_match_handler
  for each rule:
    get patterns, new_statement_pattern
    
    continuation (newdict matched_statements): 
      new_statement = substitute newdict into new_statement_pattern
      on_match_handler (knowledge new_statement)
      return #f
    
    match-multiple knowledge patterns '() '() continuation
    
  keep going until no more new knowledge added
  
//pm:match-multiple: like match-combinators, except it will match 
match-multiple knowledge patterns dict matched_statements cont
  if no patterns
    return (cont dict matched_statements)
  else    
    for each statement in knowledge
      (define (match-combinators-cont newdict n))
        (match-multiple knowledge (cdr patterns) newdict (append matched_statements statement) cont))
    
      x = match-combinators (car patterns) statement dict match-combinators-cont
      if x != false
        return x  //breaks
    return #f
|#

(define (pm:sub-dict-into-pattern dict pattern)
  'TODO)

(define (pm:match-multiple nowledge patterns dict matched_statements cont)
  (if (equal? (length patterns) 0)
    ; true
    (cont dict matched_statements)
    
    ; false
    (begin
      (call/cc (lambda (return)
        (for-each2 knowledge (lambda (statement)
          (define (cont-match-combinators newdict n)
            (pm:match-multiple (cdr patterns) newdict (append matched_statements statement) cont))
          
          (let ((x ((match:->combinators (car patterns)) statement dict cont-match-combinators)))
            (if x (return x)))
        ))
        (return #f)
      )))
    ))

(define (pm:match knowledge rules on_match_handler)
  (let ((old_knowledge_size (length knowledge)))
  
    (for-each2 rules (lambda(rule)
      (let ((patterns (car rule))
            (new_statement_pattern (cdr rule)))
      
        (define (cont-match-multiple newdict matched_statements)
          (let ((new_statement (pm:sub-dict-into-pattern newdict new_statement_pattern)))
            (on_match_handler knowledge matched_statements new_statement)
            #f))
        
        (pm:match-multiple knowledge patterns '() '() cont-match-multiple)
      )
    ))
  
    ; if knowledge changed, repeat pm:match
    (if (> (length knowledge) old_knowledge_size)
      (pm:match knowledge rules on_match_handler))))

;;; Tests
(pp "------------------------------------------")

(load "simple_data/knowledge.scm")
(load "simple_data/rules.scm")

(define (on_match knowledge matched_statements new_statement)
  (pp (list "matched" matched_statements "=>" new_statement)))

(pm:match knowledge rules on_match)

(pp "pattern_matcher done")

#|
((match:->combinators '(a (?? x) (?? y) (?? x) c))
 '((a b b b b b b c))
 '()
 (lambda (dict n)
   (pp `(succeed ,dict))
   #f))
;(succeed ((y (b b b b b b)) (x ())))
;(succeed ((y (b b b b)) (x (b))))
;(succeed ((y (b b)) (x (b b))))
;(succeed ((y ()) (x (b b b))))
;Value: #f
|#