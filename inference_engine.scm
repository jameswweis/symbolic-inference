;;;; inference_engine.scm
#|
The inference engine takes a set of knowledge and a set of rules
and makes inferences by applying the set of rules as many times as
possible to the knowledge to obtain new statements. These "inferred
statements" are added to the existing set of knowledge, and the rules
are again applied against the growing set of knowledge until no new
inferences can be made. The inference engine API contains functions to add knowledge and rules,
to start the infering, and to query the set of knowledge.

Each knowledge is made of:
- a clause (like CAUSE, BLOCK, UPREGULATES, DOWNREGULATES)
- a list of arguments (like moleculeA, moleculeB)
- a context - metadata about the knowledge, including among other things,
  the author of the paper, the data of publication, and the journal.
  The context of inferred statements includes which statements
  led to that inference (like a "taint").
  
Each rule is made of:
- a set of matching patterns - these must be matched by existing statements
- a "rewrite rule" used to generate a new knowledge statement

* Example knowledge and rules are given in the data/ folder.

The inference engine supports aliases. See [TODO: section]
of report.pdf for an explanation.

Interface:
- (ie:init)
- (ie:add-knowledge knowledge)
- (ie:add-aliases new-aliases)
- (ie:add-rules new-rules)
- (ie:print-knowledge)
- (ie:is-true statement context_predicate)
- (ie:infer context_predicate)
|#

(load "load")
(load "pattern_matcher")

(define all-knowledge)
(define all-rules)
(define all-compound_obj_aliases)

; Initializes inference engine. Call it before using other functions
(define (ie:init)
  (set! all-knowledge '())
  (set! all-rules '())
  (set! all-compound_obj_aliases '()))

; Adds the knowledge (a list) to the existing set of knowledge
(define (ie:add-knowledge knowledge)
  (set! all-knowledge (append all-knowledge knowledge)))
  
; Adds the aliases (a list) to the existing set of aliases.
(define (ie:add-aliases new-aliases)
  (set! all-compound_obj_aliases (append all-compound_obj_aliases new-aliases)))

; Adds the rules (a list) to the existing set of rules.
(define (ie:add-rules new-rules)
  (set! all-rules (append all-rules new-rules)))
  
; Prints out all the knowledge
(define (ie:print-knowledge)
  (pp all-knowledge))

; Makes inferences on the existing knowledge and prints whether the
; given statement is true. context_predicate is currently ignored.
; Example usage: (ie:is-true '(CAUSE (rain water)) '())
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

; Makes all possible inferences on the existing knowledge.
; context_predicate is currently ignored.
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
 
; 
; ie:infer helpers
;

; Returns true if new_knowledge contains something with the same
; statement (clause and args) as that of the specified knowledge.
(define (ie:has-statement knowledge new_knowledge)
  (define (statement-of k)
    (cons (car k) (cadr k)))

  (if (null? knowledge)
    #f
    (or (equal? (statement-of new_knowledge) (statement-of (car knowledge)))
        (ie:has-statement (cdr knowledge) new_knowledge))))

; Adds the new_knowledge to the existing knowledge if the existing
; knowledge does not already include it. When determining whether
; new_knowledge exists already, it ignores the contexts and compares
; only the statements (clause and args).
(define (ie:add-knowledge-in-place new-knowledge)
  (if (not (ie:has-statement all-knowledge new-knowledge))
    (append-to-end! all-knowledge new-knowledge)))

;
; ie:is-true helpers
;

; for formatting output
(define (impressive-print matches)
  (displayln "TRUE")
  (if (equal? "inferred_from" (car (car (cadr (cdr (car matches))))))
    (ip:inferred (cdr (car (cadr (cdr (car matches))))))
    (ip:original (cdr (car matches)))))

; for formatting output
(define (ip:inferred statements)
  (display "Your statement is correct and was inferred from the following ")
  (display (/ (length statements) 3))
  (displayln " statements:")
  (pp statements))

; for formatting output
(define (ip:original statement)
  (displayln "Your statement is correct and was inferred from the following statement:")
  (pp statement))

; Searches current-knowledge and returns a list of knowledge
; that matches the given statement.
(define (ie:member statement current-knowledge)
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

  (ie:member-helper statement current-knowledge '()))

; Returns true if sArg equals kArg using "alias equality"
(define (ie:eq-arg? sArg kArg)
  (if (not (list? all-compound_obj_aliases))
    (equal? sArg kArg)
    (cond ((string? kArg) (equal? kArg sArg))
          ((symbol? kArg)
            (let ((alias_list (assoc sArg all-compound_obj_aliases)))
              (or (equal? kArg sArg)
                  (and alias_list (memq? kArg (cdr alias_list)) ))))
          (else (equal? kArg sArg)))))

; Returns true if all the arguments in args1 equal all the arguments
; in args2 using "alias equality"
(define (ie:eq-args? args1 args2)
  (cond ((not (equal? (length args1) (length args2))) #f)
        ((and (null? args1) (null? args2)) #t)
        ((not (ie:eq-arg? (car args1) (car args2))) #f)
        (else (ie:eq-args? (cdr args1) (cdr args2)))))

; Returns true if the statement of (sType sArgs) equals the statement
; of (kType kArgs). The types are statement clauses and args are compared
; with "alias equality"
(define (ie:eq-statement-knowledge? sType sArgs kType kArgs)
  (and (equal? sType kType) (equal? (length sArgs) (length kArgs)) (ie:eq-args? sArgs kArgs)))
