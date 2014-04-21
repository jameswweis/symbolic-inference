;;;; main.scm

;; Load the data (rules, knowledge, contradictions)
(cd "./simple_data")
(load "rules.scm")
(load "knowledge.scm")
(load "contradictions.scm")

;; Load the inference engine
(cd "../")
(load "inference_engine.scm")
