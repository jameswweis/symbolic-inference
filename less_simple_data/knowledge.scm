(define compound_obj_aliases (list
  (cons "shooting guards" (list 'kobe 'wade 'mj 'jordan))
))

(define simple-context
  (list
   (cons "title" "title1")
   (cons "author" "author1")
   (cons "year" "year1")
   (cons "university" "univ1")
   (cons "topic" "topic1")
   (cons "journal" "journal1")
   (cons "pubmed" "pubmed1")
   (cons "locations" (list "loc_a1" "loc_b1"))
   ))

(define knowledge
  (list
   (list 'CAUSE '(thing-1 thing-2) simple-context)
   (list 'CAUSE '(thing-2 thing-3) simple-context)
   (list 'CAUSE '(thing-2 thing-4) simple-context)
   (list 'UPREGULATE '(monster-1 monster-2) simple-context)
   (list 'DOWNREGULATE '(monster-2 monster-3) simple-context)
   (list 'DOWNREGULATE '(monster-2 monster-4) simple-context)
   ))

(pp compound_obj_aliases)
(pp knowledge)
(pp "knowledge done")
