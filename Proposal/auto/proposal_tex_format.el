(TeX-add-style-hook "proposal_tex_format"
 (lambda ()
    (LaTeX-add-bibitems
     "trapnell10"
     "grabherr11"
     "pell12"
     "sharon13"
     "rehurek20"
     "rousseeuw87"
     "rand71"
     "strehl02")
    (TeX-add-symbols
     '("e" 1)
     '("E" 1)
     '("Var" 1)
     '("ind" 1)
     '("compl" 1)
     '("setof" 2)
     '("set" 1)
     '("ital" 1)
     '("new" 1)
     '("problem" 1)
     "thisclass"
     "thishwshort"
     "thishwlong"
     "myname"
     "mylogin"
     "duedate"
     "C"
     "N"
     "Q"
     "R"
     "Z")
    (TeX-run-style-hooks
     "verbatim"
     "fullpage"
     "bbm"
     "hyperref"
     "amstext"
     "inputenc"
     "utf8"
     "graphicx"
     "enumerate"
     "latexsym"
     "amssymb"
     "amsfonts"
     "amsmath"
     ""
     "latex2e"
     "art11"
     "article"
     "11pt")))

