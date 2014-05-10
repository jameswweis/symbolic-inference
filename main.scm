;;;; main.scm 

; Demos and tests are located here.

(load "inference_engine.scm")
(load "linear_estimator.scm")

(ie:init)

; Demo of inferences on cancer biology data/rules
; All the inferences for the cancer biology knowledge are stored
; in data/cancer_biology/inferences.txt.
; This takes 6 minutes to run and generates 2000 lines of text.
(define (cancer-biology-demo)
  (load "data/cancer_biology/knowledge.scm")
  (load "data/cancer_biology/rules.scm")
  (ie:add-knowledge knowledge)
  (ie:add-aliases compound_obj_aliases)
  (ie:add-rules rules)
  (ie:infer '())
  (ie:print-knowledge))
#|
(cancer-biology-demo)
|#


; Demo of linear estimator which automatically generates knowledge
; from tables of molecule concentrations.
(define (linear-estimator-demo)
  ;; Add knowledge from data tables using linear estimator
  (ie:add-knowledge
    (list (le:knowledge-from "./data/cancer_biology/p15_cyclin-d_AND_cdk4.txt" '())))
  (ie:add-knowledge
    (list (le:knowledge-from "./data/cancer_biology/cyclin-d_AND_cdk4_rb.txt" '())))
  (ie:add-knowledge
    (list (le:knowledge-from "./data/cancer_biology/rb_e2fs.txt" '())))
  (ie:add-knowledge
    (list
      (le:knowledge-from "./data/cancer_biology/e2fs_changes-in-gene-expression.txt" '())))
  (ie:print-knowledge))
#|
(linear-estimator-demo)
((block (p15 cyclin-d:cdk4) ())
 (block (cyclin-d:cdk4 rb) ())
 (block (rb e2fs) ())
 (cause (e2fs changes-in-gene-expressions) ()))
|#


; demo of inferences on small set of knowledge/rules/aliases
(define (simple-demo)
  (load "data/simple/knowledge.scm")
  (load "data/simple/rules.scm")
  (ie:add-knowledge knowledge)
  (ie:add-aliases compound_obj_aliases)
  (ie:add-rules rules)
  (ie:is-true '(CAUSE ("shooting guards" win)) '()))
#|
(simple-demo)
TRUE. Your statement is correct and was inferred from the following 2 statements:
(cause
 ("shooting guards" point)
 (("inferred_from"
   cause
   ("shooting guards" score)
   (("title" . "title1") ("author" . "author1")
                         ("year" . "year1")
                         ("university" . "univ1")
                         ("topic" . "topic1")
                         ("journal" . "journal1")
                         ("pubmed" . "pubmed1")
                         ("locations" "loc_a1" "loc_b1"))
   cause
   (score point)
   (("title" . "title2") ("author" . "author2")
                         ("year" . "year2")
                         ("university" . "univ2")
                         ("topic" . "topic2")
                         ("journal" . "journal2")
                         ("pubmed" . "pubmed2")
                         ("locations" "loc_a2" "loc_b2"))))
 cause
 (point win)
 (("title" . "title3") ("author" . "author3")
                       ("year" . "year3")
                       ("university" . "univ3")
                       ("topic" . "topic3")
                       ("journal" . "journal3")
                       ("pubmed" . "pubmed3")
                       ("locations" "loc_a3" "loc_b3")))
|#


; Unit test for pm:match
#|
(load "data/simple/knowledge.scm")
(load "data/simple/rules.scm")

(define (on_match knowledge matched_statements new_statement)
  (pp (list "!!!!! on_match" matched_statements "=>" new_statement)))

(pm:match knowledge rules on_match compound_obj_aliases)
|#