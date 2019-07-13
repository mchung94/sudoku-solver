(defsystem "sudoku-solver"
  :description "A Sudoku puzzle solver that can work with irregular boxes."
  :author "Mitchell Chung"
  :license "MIT"
  :pathname "src/"
  :serial t
  :components
  ((:file "packages")
   (:file "sudoku-solver")
   (:file "gui"))
  :in-order-to ((test-op (test-op "sudoku-solver/tests"))))

(defsystem "sudoku-solver/tests"
  :description "Tests for the Sudoku solver."
  :depends-on ("sudoku-solver" "1am")
  :pathname "tests/"
  :components ((:file "sudoku-solver-tests"))
  :perform (test-op (o c) (symbol-call :1am '#:run)))
