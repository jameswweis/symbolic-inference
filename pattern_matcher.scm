;;;; pattern_matcher.scm

(load "load")

; load modified version of GJS matcher
(load "ghelper")
(load "matcher")

; Pseudocode in pattern_matcher_pseudocode.txt

(define (pm:sub-dict-into-pattern dict pattern)  
  (define (tree-copy-with-sub tree)
    (let loop ((tree tree))
      (if (pair? tree)
        (if (equal? (car tree) '?)
          (cadr (assoc (cadr tree) dict))
          (cons (loop (car tree)) (loop (cdr tree))))
        tree)))
  
  (tree-copy-with-sub pattern))

(define (pm:match-multiple knowledge patterns dict matched_statements cont)
  (if (equal? (length patterns) 0)
    (begin 'true-block
      (cont dict matched_statements))

    (begin 'false-block
      (call/cc (lambda (return)
        (for-each2 knowledge (lambda (statement)
          (define (cont-match-combinators newdict n)
            ;(pp `(individual-succeed ,newdict))
            (pm:match-multiple knowledge (cdr patterns) newdict (append matched_statements statement) cont))
          
          ;(pp (list "matching pattern:" (car patterns) "against:" (list (cons (car statement) (cadr statement)))  ))
          
          (let* ((clause_and_args (list (cons (car statement) (cadr statement))))
                 (x ((match:->combinators (car patterns)) clause_and_args dict cont-match-combinators)))
            (if x (return x)))))
        (return #f))))))

(define (pm:match knowledge rules on_match_handler aliases)
  (let ((old_knowledge_size (length knowledge)))
    (match:set-compound_obj_aliases! aliases)
    
    (for-each2 rules (lambda(rule)
      (let ((patterns (car rule))
            (new_statement_pattern (cdr rule)))
      
        (define (cont-match-multiple newdict matched_statements)
          (let ((new_statement (pm:sub-dict-into-pattern newdict new_statement_pattern)))
            (on_match_handler knowledge matched_statements new_statement)
            #f))
        
        (pm:match-multiple knowledge patterns '() '() cont-match-multiple))))
  
    ; if knowledge changed, repeat pm:match
    (if (> (length knowledge) old_knowledge_size)
      (pm:match knowledge rules on_match_handler aliases))))
