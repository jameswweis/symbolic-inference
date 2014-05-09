;;;; main.scm

(load "inference_engine.scm")

(ie:init)

(load "data/simple/knowledge.scm")
(load "data/simple/rules.scm")

(ie:add-knowledge knowledge)
(ie:add-aliases compound_obj_aliases)
(ie:add-rules rules)

(ie:is-true (list 'CAUSE (list "shooting guards" 'win)) '())
;(ie:infer '())
;(ie:print-knowledge)