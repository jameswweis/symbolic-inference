;;;; inference_engine.scm

(load "pattern_matcher.scm")

(define knowledge)

(define rules)

(define (ie:init)
  (set! knowledge '())
  (set! rules '()))

(define (ie:add-knowledge new-knowledge)
  (append new-knowledge knowledge))

(define (ie:add-aliases new-aliases)
  (append new-aliases compound_obj_aliases))

(define (ie:add-rules new-rules)
	(append new-rules rules))

