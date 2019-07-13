(in-package #:sudoku-solver)

;;; A representation of a matrix of 0s and 1s (for the DLX algorithm)

(defclass data ()
  ((left :accessor left)
   (right :accessor right)
   (up :accessor up)
   (down :accessor down)
   (column :accessor column :initarg :column))
  (:documentation "Represents a 1 in a matrix of 0s and 1s.
Matrix rows are circular doubly linked lists using left/right.
Matrix columns are circular doubly linked lists using up/down.
The column slot points to a column header which is also part of the up/down list."))

(defclass column (data)
  ((size :accessor size :initform 0)
   (name :reader name :initarg :name))
  (:documentation "A matrix column header or root object.
The root object plus the columns are all circularly connected using left/right.
Each column uses up/down to link with data objects representing the column's 1s."))

(defmethod initialize-instance :after ((data data) &key right-data column)
  "Add DATA to the left of RIGHT-DATA and above COLUMN, otherwise loop to itself."
  (if right-data
      (setf (left data) (left right-data)
            (right data) right-data
            (left (right data)) data
            (right (left data)) data)
    (setf (left data) data (right data) data))
  (if column
      (setf (up data) (up column)
            (down data) column
            (up (down data)) data
            (down (up data)) data
            (size column) (1+ (size column)))
    (setf (up data) data (down data) data)))

(defmethod initialize-instance :after ((column column) &key)
  "Set the newly created COLUMN's column to itself."
  (setf (column column) column))

(defun make-empty-matrix (column-names)
  "Return the root of a new matrix with the given COLUMN-NAMES but no rows."
  (let ((root (make-instance 'column :name "Root")))
    (dolist (name column-names root)
      (make-instance 'column :name name :right-data root))))

(defun add-row (columns)
  "Add a row to the matrix where COLUMNS are the columns with a 1."
  (let ((first-data (make-instance 'data :column (first columns))))
    (dolist (c (rest columns))
      (make-instance 'data :column c :right-data first-data))))

(defmacro do-matrix ((var start next-accessor end &optional result-form) &body body)
  "Loop with VAR set from START to END, updated by NEXT-ACCESSOR. Execute BODY
with VAR set to each value up to but not including END. Return RESULT-FORM."
  `(do ((,var ,start (,next-accessor ,var)))
       ((eq ,var ,end) ,result-form)
     ,@body))

(defun matrix-columns (matrix-root)
  "Return a list of the matrix's columns."
  (let ((columns '()))
    (do-matrix (c (right matrix-root) right matrix-root (nreverse columns))
      (push c columns))))



;;; An implementation of the DLX algorithm for solving exact cover problems

(defun choose-column (matrix-root)
  "Return the column with the smallest size."
  (let ((smallest (right matrix-root)))
    (do-matrix (c (right smallest) right matrix-root smallest)
      (when (< (size c) (size smallest))
        (setf smallest c)))))

(defun cover-column (c)
  "Remove column C from the matrix, and all rows with a 1 in C."
  (setf (left (right c)) (left c) (right (left c)) (right c))
  (do-matrix (i (down c) down c)
    (do-matrix (j (right i) right i)
      (setf (up (down j)) (up j) (down (up j)) (down j))
      (decf (size (column j))))))

(defun uncover-column (c)
  "Undo COVER-COLUMN: add column C and rows with a 1 in C back into the matrix."
  (do-matrix (i (up c) up c)
    (do-matrix (j (left i) left i)
      (incf (size (column j)))
      (setf (up (down j)) j (down (up j)) j)))
  (setf (left (right c)) c (right (left c)) c))

(defun dlx-search (matrix-root &optional solution)
  "Look for a set of rows in the matrix with exactly one 1 in each column.
Return NIL if there's no solution, or a list of data objects, one for each
row of the solution. This stops after finding any solution, instead of
searching for all solutions."
  (when (eq (right matrix-root) matrix-root)
    (return-from dlx-search solution))
  (let ((c (choose-column matrix-root)))
    (cover-column c)
    (do-matrix (r (down c) down c)
      (push r solution)
      (do-matrix (j (right r) right r)
        (cover-column (column j)))
      (let ((result (dlx-search matrix-root solution)))
        (when result
          (return-from dlx-search result)))
      (do-matrix (j (left r) left r)
        (uncover-column (column j)))
      (pop solution))
    (uncover-column c))
  nil)



;;; Sudoku puzzle solver with support for irregular-shaped boxes/regions.

(deftype grid-value ()
  "Valid values in Sudoku grids. 0 means an empty cell."
  '(integer 0 9))

(deftype grid ()
  "A 9x9 2D array to represent a Sudoku grid."
  '(simple-array grid-value (9 9)))

(defun make-empty-grid ()
  "Create and return an empty grid."
  (make-array '(9 9) :element-type 'grid-value :initial-element 0))

(defun copy-grid (grid)
  "Create and return a copy of the grid."
  (let ((new-grid (make-empty-grid)))
    (dotimes (row 9 new-grid)
      (dotimes (col 9)
        (setf (aref new-grid row col) (aref grid row col))))))

(defun box-grid-mistakes (box-grid)
  "Return a list of (row . column) conses for bad cell values in a box grid.
Box grids use the digits 1-9 to indicate the different boxes/regions that must
contain the digits 1-9 (each row, column, and box must contain 1-9 in the
Sudoku solution). In a regular Sudoku puzzle, the boxes are nine 3x3 regions,
but Microsoft Sudoku supports irregular-shaped boxes. Cell values are bad when
they're empty, not next to another cell with the same digit, or if there are not
exactly nine of the same digit."
  (labels ((neighbors (row col)
             (loop for (ro . co) in '((-1 . -1) (-1 . 0) (-1 . 1) (0 . -1) (0 . 1) (1 . -1) (1 . 0) (1 . 1))
                   for r = (+ row ro)
                   for c = (+ col co)
                   when (and (<= 0 r 8) (<= 0 c 8))
                   collect (cons r c)))
           (separatep (row col box)
             (loop for (r . c) in (neighbors row col)
                   never (= box (aref box-grid r c))))
           (box-cell-counts ()
             (let ((counts (make-array 10 :initial-element 0)))
               (dotimes (row 9 counts)
                 (dotimes (col 9)
                   (incf (svref counts (aref box-grid row col))))))))
    (loop with counts = (box-cell-counts)
          for row from 0 below 9
          nconc (loop for col from 0 below 9
                      for box = (aref box-grid row col)
                      when (or (zerop box)
                               (separatep row col box)
                               (/= 9 (svref counts box)))
                      collect (cons row col)))))

(defun sudoku-grid-mistakes (sudoku-grid box-grid)
  "Return a list of (row . column) conses for bad cell values in a Sudoku grid.
Cell values are bad when there is another cell in the same row/column/box with
the same digit."
  (flet ((count-in-column (col digit)
           (loop for row from 0 below 9
                 count (= digit (aref sudoku-grid row col))))
         (count-in-row (row digit)
           (loop for col from 0 below 9
                 count (= digit (aref sudoku-grid row col))))
         (count-in-box (box digit)
           (loop for row from 0 below 9
                 sum (loop for col from 0 below 9
                           count (and (= box (aref box-grid row col))
                                      (= digit (aref sudoku-grid row col)))))))
    (loop for row from 0 below 9
          nconc (loop for col from 0 below 9
                      for digit = (aref sudoku-grid row col)
                      for box = (aref box-grid row col)
                      when (and (not (zerop digit))
                                (or (> (count-in-column col digit) 1)
                                    (> (count-in-row row digit) 1)
                                    (> (count-in-box box digit) 1)))
                      collect (cons row col)))))

(defun string-to-grid (string)
  "Create a grid from a string. 0 and period are empty cells, 1-9 are digits,
ignore all other characters. Any unassigned cells default to empty."
  (loop with grid = (make-empty-grid)
        for ch across (remove-if (complement #'digit-char-p) (substitute #\0 #\. string))
        for index from 0
        do (setf (row-major-aref grid index) (position ch "0123456789"))
        finally (return grid)))

(defvar *default-box-grid*
  (string-to-grid "1 1 1 | 2 2 2 | 3 3 3
                   1 1 1 | 2 2 2 | 3 3 3
                   1 1 1 | 2 2 2 | 3 3 3
                   ------+-------+------
                   4 4 4 | 5 5 5 | 6 6 6
                   4 4 4 | 5 5 5 | 6 6 6
                   4 4 4 | 5 5 5 | 6 6 6
                   ------+-------+------
                   7 7 7 | 8 8 8 | 9 9 9
                   7 7 7 | 8 8 8 | 9 9 9
                   7 7 7 | 8 8 8 | 9 9 9")
  "The 3x3 regions that must contain the digits 1-9 in regular Sudoku.")

(defvar *constraint-names*
  (nconc (loop for row from 0 below 9
               nconc (loop for col from 0 below 9
                           collect (list :row row :col col)))
         (loop for row from 0 below 9
               nconc (loop for digit from 1 to 9
                           collect (list :row row :digit digit)))
         (loop for col from 0 below 9
               nconc (loop for digit from 1 to 9
                           collect (list :col col :digit digit)))
         (loop for box from 1 to 9
               nconc (loop for digit from 1 to 9
                           collect (list :box box :digit digit))))
  "The 324 constraints that must be satisfied for a valid Sudoku solution:
0-80: there is a value in the cell at (row, col)
81-161: row (0-8) has the digit (1-9)
162-242: column (0-8) has the digit (1-9)
243-323: box (1-9) has the digit (1-9)")

(defun get-columns (columns row col box digit)
  "Return the four columns representing the constraints satisfied by having
digit in the given row/col/box."
  (list (svref columns (+ (* row 9) col))
        (svref columns (+ (* row 9) (1- digit) (* 9 9 1)))
        (svref columns (+ (* col 9) (1- digit) (* 9 9 2)))
        (svref columns (+ (* (1- box) 9) (1- digit) (* 9 9 3)))))

(defun solve (sudoku-grid &optional (box-grid *default-box-grid*) (solution-grid (make-empty-grid)))
  "Solve the SUDOKU-GRID puzzle where the boxes are defined by BOX-GRID.
Overwrite SOLUTION-GRID with the solution if available, otherwise set it
the same values as SUDOKU-GRID. Returns SOLUTION-GRID."
  (let* ((root (make-empty-matrix *constraint-names*))
         (columns (coerce (matrix-columns root) 'vector)))
    (dotimes (row 9)
      (dotimes (col 9)
        (loop with box = (aref box-grid row col)
              for digit from 1 to 9
              do (add-row (get-columns columns row col box digit)))))
    (dotimes (row 9)
      (dotimes (col 9)
        (let ((digit (aref sudoku-grid row col))
              (box (aref box-grid row col)))
          (setf (aref solution-grid row col) digit)
          (unless (zerop digit)
            (mapc #'cover-column (get-columns columns row col box digit))))))
    (flet ((set-digit (&key row col box digit)
               (declare (ignore box))
               (setf (aref solution-grid row col) digit))
           (row-names (data)
             (loop for d = data then (right d)
                   repeat 4
                   append (name (column d)))))
      (dolist (data (dlx-search root) solution-grid)
        (apply #'set-digit (row-names data))))))
