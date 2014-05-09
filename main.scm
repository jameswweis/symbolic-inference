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


;; Add knowledge from data tables using linear estimator
(ie:add-knowledge
 (list (le:knowledge-from "./data/cancer_biology/p15_cyclin-d:cdk4.txt" '())))

(ie:add-knowledge
 (list (le:knowledge-from "./data/cancer_biology/cyclin-d:cdk4_rb.txt" '())))

(ie:add-knowledge
 (list (le:knowledge-from "./data/cancer_biology/rb_e2fs.txt" '())))

(ie:add-knowledge
 (list
  (le:knowledge-from "./data/cancer_biology/e2fs_changes-in-gene-expression.txt"
                     '())))
