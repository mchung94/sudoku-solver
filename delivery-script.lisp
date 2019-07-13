;;;; LispWorks-specific delivery script
;;;; Review carefully if using this script on your own system, because it
;;;; loads the default LispWorks user initialization file and expects a recent
;;;; version of ASDF and Quicklisp to be loaded by it, and expects the current
;;;; directory to be the same directory this file is in.

;;; Make sure to load all patches before delivery
(in-package "CL-USER")
(load-all-patches)

;;; Load the LispWorks user initialization file
(load *init-file-name*)

;;; Delete the dist/ directory to remove any previous builds and recreate it
(sys:call-system "rd /s /q dist")
(ensure-directories-exist "dist/")
(change-directory "dist/")

;;; Load the application
(push "../" asdf:*central-registry*)
(asdf:load-system "sudoku-solver")
(copy-file "../README.md" "README.md")

;;; Deliver the application, sudoku-solver.exe
(deliver 'sudoku-solver:main "sudoku-solver.exe" 5
         :interface :capi
         :icon-file "../solver.ico"
         :startup-bitmap-file nil)
