;; Biological data

(define compound_obj_aliases '())

(define wnt-pathway-context
  (list
   (cons "title" "The Wnt signalling pathway")
   (cons "author" "Huelsken, J., and Juergen, B.")
   (cons "year" "2000")
   (cons "university" "Max Delbrueck Center for Molecular Medicine")
   (cons "topic" "Cell biology")
   (cons "journal" "J. Cell Sci.")
   (cons "pubmed" "Unknown.")
   (cons "locations" (list "loc_a1" "loc_b1"))))

(define hormone-context
  (list
   (cons "title" "The Hallmarks of Cancer")
   (cons "author" "Hanahan, D., and Weinberg, R.A.")
   (cons "year" "2000")
   (cons "university" "University of California at San Francisco")
   (cons "topic" "Cancer")
   (cons "journal" "Cell")
   (cons "pubmed" "10647931")
   (cons "locations" (list "loc_a1" "loc_b1"))))

(define cytokine-context
  (list
   (cons "title" "The Hallmarks of Cancer")
   (cons "author" "Hanahan, D., and Weinberg, R.A.")
   (cons "year" "2000")
   (cons "university" "University of California at San Francisco")
   (cons "topic" "Cancer")
   (cons "journal" "Cell")
   (cons "pubmed" "10647931")
   (cons "locations" (list "loc_a1" "loc_b1"))))

(define tgfb-context
  (list
   (cons "title" "TGF-Beta signaling from cell membrane to nucleus through SMAD proteins.")
   (cons "author" "Heldin, C., Miayazono, K., and Dijke, P.")
   (cons "year" "1997")
   (cons "university" "Ludwig Institute for Cancer Research")
   (cons "topic" "Cell biology")
   (cons "journal" "Nature")
   (cons "pubmed" "9393997")
   (cons "locations" (list "loc_a1" "loc_b1"))))

(define knowledge
  (list
   ;; Wnt signaling pathway
   '(CAUSE (WNT Frizzled WNT:Frizzled) wnt-pathway-context)
   '(CAUSE (WNT:Frizzled Dishevelled) wnt-pathway-context)
   '(CAUSE (Dishevelled GSK-3Beta) wnt-pathway-context)
   '(CAUSE (GSK-3Beta APC) wnt-pathway-context)
   '(CAUSE (APC Beta-Cetenin) wnt-pathway-context)
   '(CAUSE (Other-cell E-Cadherin Beta-Catenin) wnt-pathway-context)
   '(CAUSE (Beta-Catenin TCF Beta-Catenin:TCF) wnt-pathway-context)
   '(CAUSE (Beta-Catenin:TCF Changes-in-gene-expression))

   ;; Hormone pathway
   '(CAUSE (Estrogen ER) hormone-context)
   '(CAUSE (ER Changes-in-gene-expression) hormone-context)
   '(CAUSE (Bombesin 7-TMR G-protein) hormone-context)
   '(CAUSE (G-protein Ad-Cyclin) hormone-context)
   '(CAUSE (Ad-Cyclin PKA) hormone-context)
   '(CAUSE (PKA CREB) hormone-context)
   '(CAUSE (CREB Changes-in-gene-expression) hormone-context)

   ;; Cytokine Pathway
   '(CAUSE (Cytokines Cytokine-R Jaks) cytokine-context)
   '(CAUSE (Jaks Stat3) cytokine-context)
   '(CAUSE (Jaks Stat5) cytokine-context)
   '(CAUSE (Stat3 BclXL) cytokine-context)
   '(CAUSE (Stat5 BclXL) cytokine-context)
   '(CAUSE (Stat3 Changes-in-gene-expression) cytokine-context)
   '(CAUSE (Stat4 Changes-in-gene-expression) cytokine-context)

   ;; TGF-Beta Pathway
   '(CAUSE (TGF-Beta TGF-Beta-R TGF-Beta:TGF-Beta-R) tgfb-context)
   '(CAUSE (TGF-Beta:TGF-Beta-R SMADs) tgfb-context)
   '(CAUSE (SMADs p15) tgfb-context)
   '(CAUSE (Cyclin-D CDK4 Cyclin-D:CDK4) tgfb-context)
   '(BLOCK (p16 Cyclin-D:CDK4) tgfb-context)
   '(BLOCK (p15 Cyclin-D:CDK4) tgfb-context)
   '(BLOCK (Cyclin-D:CDK4 Rb) tgfb-context)
   '(BLOCK (HPV-E7 Rb) tgfb-context)
   '(BLOCK (Rb E2Fs) tgfb-context)
   '(BLOCK (E2Fs Changes-in-gene-expression) tgfb-context)
   '(CAUSE (SMADs p27) tgfb-context)
   '(CAUSE (Cyclin-E CDK2 Cyclin-E:CDK2) tgfb-context)
   '(BLOCK (p27 Cyclin-E:CDK2) tgfb-context)
   '(CAUSE (Cyclin-E:CDK2 Cell-proliferation) tgfb-context)
   '(CAUSE (Changes-in-gene-expression Cyclin-E:CDK2) tgfb-context)
   '(CAUSE (SMADs p21) tgfb-context)
   '(CAUSE (Cyclin-E CDK2 Cyclin-E:CDK2) tgfb-context)
   '(BLOCK (p21 Cyclin-E:CDK2) tgfb-context)
   ))


(pp compound_obj_aliases)
(pp knowledge)
(pp "knowledge done")
