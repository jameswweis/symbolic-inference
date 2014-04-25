;;;; inference_engine.scm


(load "pattern_matcher.scm")

(define all-knowledge)

(define all-rules)

(define (ie:init)
  (set! all-knowledge '())
  (set! all-rules '()))


(define (ie:add-knowledge new-knowledge)
  (set! all-knowledge
        (append new-knowledge all-knowledge)))

(define (ie:add-aliases new-aliases)
  (append new-aliases compound_obj_aliases))

(define (ie:add-rules new-rules)
  (append new-rules rules))

(define (ie:print-knowledge)
  (pp all-knowledge))

(define (ie:is-true statement context_predicate)
	(pm:match all-knowledge all-rules)

	;Exact context
	(if (not context_predicate)
		(if (member (cons statement context_predicate) all-knowledge)
				(pp "true ") 	;TODO: return relevant paper info
				(pp "false"))


		)
	
	)

;; Tests
(load "./simple_data/knowledge.scm")
(pp knowledge)

(ie:init)
(ie:print-knowledge)
(ie:add-knowledge knowledge)
(ie:print-knowledge)
