;(ie:add-rules new-rules);;;; main.scm

(load "inference_engine.scm")

(load "data/cancer_biology/knowledge.scm")
(load "data/cancer_biology/rules.scm")

(ie:init)

(ie:add-knowledge2 knowledge)
(ie:add-rules rules)

;(ie:is-true2 (list 'CAUSE (list "shooting guards" 'win)) '())
(ie:infer '())
(ie:print-knowledge)