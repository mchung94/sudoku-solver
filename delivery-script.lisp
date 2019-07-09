;;;; LispWorks-specific delivery script

(in-package "CL-USER")
(load-all-patches)

(compile-file "sudoku-solver.lisp" :load t)

(deliver 'main "sudoku-solver.exe" 5
         :interface :capi
         :icon-file "solver.ico"
         :startup-bitmap-file nil)
