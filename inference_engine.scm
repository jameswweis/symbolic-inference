;;;; inference_engine.scm

(load "simple_data/knowledge.scm")
(define all-knowledge)
(define all-rules)

(define (ie:init)
  (set! all-knowledge '())
  (set! all-rules '())
  (set! compound_obj_aliases '()))


(define (ie:add-knowledge new-knowledge)
  (let ((filtered-new-knowledge
         (filter (lambda (x) (not (member x all-knowledge))) new-knowledge)))
    (set! all-knowledge (append all-knowledge filtered-new-knowledge))))

(define (ie:add-aliases new-aliases)
  (append! new-aliases compound_obj_aliases))

(define (ie:add-rules new-rules)
  (append! new-rules rules))

(define (ie:print-knowledge)
  (pp all-knowledge))

(define (ie:is-true statement context_predicate)
  (pp (ie:member statement all-knowledge)))

(define (ie:member statement current-knowledge)

  (if (null? current-knowledge) #f 
    (let*  (
      (statementType (car statement)) 
      (statementArgs (car (cdr statement))) 
      (knowledgeType (car (car current-knowledge))) 
      (knowledgeArgs (car (cdr (car current-knowledge)))))


      (cond 
        ((equal? statementType knowledgeType)
          (if (ie:eqvArgs? statementArgs knowledgeArgs) ; TODO: aliases
            #t 
            (ie:member statement (cdr current-knowledge))))
        (else (ie:member statement (cdr current-knowledge)))))))

(define (ie:eqvArgs? statementArgs knowledgeArgs)
  (cond 
    ((not (= (length statementArgs) (length knowledgeArgs))) #f)
    ((and (null? statementArgs) (null? knowledgeArgs)) #t)
    (else 

      (let* (   (sCar (car statementArgs))
                (kCar (car knowledgeArgs))
                (sCdr (cdr statementArgs))
                (kCdr (cdr knowledgeArgs)))

      (set! sCar (expandList sCar compound_obj_aliases))
      (set! kCar (expandList kCar compound_obj_aliases))

      (if (intersect? sCar kCar) 
        (ie:eqvArgs? sCdr kCdr)
        #f)

      ))))


(define (expandList arg aliasList)
  (cond
    ((null? aliasList) arg)
    ((equal? arg (car (car (aliasList)))) (cons arg (cdr (car aliasList))))
    (else (expandList arg (cdr aliasList)))
  ))


; From: http://stackoverflow.com/questions/16692978/scheme-check-if-anything-in-two-lists-are-the-same
(define (intersect? list1 list2)
  (and (not (null? list1))
       (or (member     (car list1) list2)
           (intersect? (cdr list1) list2))))

;; Tests
(load "./simple_data/knowledge.scm")
(pp knowledge)

(ie:init)
;Value: ()

(ie:print-knowledge)
; ()

(define excess-knowledge (list
(list 'CAUSE (list "shooting guards" 'score)
      (list
       (cons "title" "title1")
       (cons "author" "author1")
       (cons "year" "year1")
       (cons "university" "univ1")
       (cons "topic" "topic1")
       (cons "journal" "journal1")
       (cons "pubmed" "pubmed1")
       (cons "locations" (list "loc_a1" "loc_b1"))))))
;Value: excess-knowledge

(ie:add-knowledge excess-knowledge)
;Value: ()

(ie:print-knowledge)
;; ((cause
;;   ("shooting guards" score)
;;   (("title" . "title1")
;;    ("author" . "author1")
;;    ("year" . "year1")
;;    ("university" . "univ1")
;;    ("topic" . "topic1")
;;    ("journal" . "journal1")
;;    ("pubmed" . "pubmed1")
;;    ("locations" "loc_a1" "loc_b1"))))

(ie:add-knowledge excess-knowledge)
;; ;Value: ((cause ("shooting guards" score)
;; (("title" . "title1") ("author" . "author1") ("year" . "year1")
;;  ("university" . "univ1") ("topic" . "topic1") ("journal" . "journal1")
;;  ("pubmed" . "pubmed1") ("locations" "loc_a1" "loc_b1"))))

(ie:print-knowledge)
;; ((cause
;;   ("shooting guards" score)
;;   (("title" . "title1")
;;    ("author" . "author1")
;;    ("year" . "year1")
;;    ("university" . "univ1")
;;    ("topic" . "topic1")
;;    ("journal" . "journal1")
;;    ("pubmed" . "pubmed1")
;;    ("locations" "loc_a1" "loc_b1"))))
