;;;; LispWorks-specific delivery script

(in-package "CL-USER")
(load-all-patches)

(ensure-directories-exist "dist/")
(change-directory "dist/")

(compile-file "../sudoku-solver.lisp" :output-file "sudoku-solver" :load t)
(copy-file "../README.md" "README.md")

(deliver 'main "sudoku-solver.exe" 5
         :interface :capi
         :icon-file "../solver.ico"
         :startup-bitmap-file nil)
