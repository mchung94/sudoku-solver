;;;; LispWorks-specific delivery script
;;;; Review this script if using this script on your own system.

;;; Make sure to load all patches before delivery
(in-package "CL-USER")
(load-all-patches)

;;; Load the LispWorks user initialization file.
;;; This file must load a recent version of ASDF and Quicklisp in order to
;;; load the project successfully.
(load *init-file-name*)

;;; Delete the dist/ directory to remove any previous builds
(uiop:delete-directory-tree (pathname "dist/")
                            :validate t
                            :if-does-not-exist :ignore)

;;; Load the application
;;; This assumes the current directory contains sudoku-solver.asd
(push (uiop/os:getcwd) asdf:*central-registry*)
(asdf:load-system "sudoku-solver")

;;; Run the tests (in 1am, failures signal errors so it won't pass this part)
(asdf:test-system "sudoku-solver")

;;; Deliver the application, sudoku-solver.exe, and its README.md
(ensure-directories-exist "dist/")
(copy-file "README.md" "dist/README.md")
(deliver 'sudoku-solver:main "dist/sudoku-solver.exe" 5
         :interface :capi
         :icon-file "solver.ico"
         :startup-bitmap-file nil)
