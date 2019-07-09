;;;; Sudoku Solver - written with LispWorks 7.1.1 64-bit on Windows 10
;;;; The Sudoku solving code is an implementation of Knuth's DLX algorithm
;;;; (Dancing Links / Algorithm X), solving Sudoku puzzles by converting them
;;;; into exact cover problems.



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

(defun grid-to-string (grid &optional add-whitespace)
  "Convert a grid into a string. Periods are empty cells, 1-9 are digits.
If ADD-WHITESPACE is true then it will return a string with spaces/newlines to
be easier to see."
  (format nil (if add-whitespace "~{~{~C~^ ~}~^~%~}" "~{~{~C~}~}")
          (loop for row from 0 below 9
                collect (loop for col from 0 below 9
                              collect (char ".123456789" (aref grid row col))))))

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



;;; GUI support code

(defclass model ()
  ((puzzle-grid :reader puzzle-grid :initform (make-empty-grid)
                :documentation "The sudoku grid for the numbers defining the puzzle.")
   (box-grid :reader box-grid :initform (copy-grid *default-box-grid*)
             :documentation "The sudoku grid for defining boxes (1-9 in each row, column, and box).")
   (solution-grid :reader solution-grid :initform (make-empty-grid)
                  :documentation "The solution to puzzle-grid + box-grid, or empty if they have problems.")
   (current-cell :accessor current-cell :initform nil
                 :documentation "NIL or a cons containing (row . col) of the currently selected cell.")
   (current-item :accessor current-item :initform nil
                 :documentation "NIL or a character for the currently selected item: 1-9 = digits, A-I = box colors.")
   (grid-mistakes :accessor grid-mistakes :initform nil
                  :documentation "A list of (row . col) conses for cells with bad numbers.")
   (box-mistakes :accessor box-mistakes :initform nil
                 :documentation "A list of (row . col) conses for cells with bad box colors."))
  (:documentation "The data for the Sudoku Solver user interaction, independent of the GUI."))

(defun puzzle-grid-numbers (model)
  "Return how many numbers are in the puzzle grid."
  (loop for i from 0 below 81
        count (plusp (row-major-aref (puzzle-grid model) i))))

(defun clear-grid (grid)
  "Remove all numbers 1-9 from a grid."
  (dotimes (row 9)
    (dotimes (col 9)
      (setf (aref grid row col) 0))))

(defun update-grids (model)
  "After changes to puzzle-grid or box-grid, update box-mistakes/grid-mistakes/solution-grid."
  (setf (box-mistakes model)
        (box-grid-mistakes (box-grid model)))
  (setf (grid-mistakes model)
        (unless (box-mistakes model)
          (sudoku-grid-mistakes (puzzle-grid model) (box-grid model))))
  ;; don't try to solve the puzzle if too few numbers are in the input or it could be slow
  (if (or (box-mistakes model) (grid-mistakes model) (< (puzzle-grid-numbers model) 15))
      (clear-grid (solution-grid model))
    (solve (puzzle-grid model) (box-grid model) (solution-grid model))))

(defun clear-all-numbers (model)
  "Remove all numbers from the puzzle grid."
  (clear-grid (puzzle-grid model))
  (update-grids model))

(defun reset-all-boxes (model)
  "Reset the grid's box colors back to the standard nine 3x3 boxes."
  (dotimes (row 9)
    (dotimes (col 9)
      (setf (aref (box-grid model) row col) (aref *default-box-grid* row col))))
  (update-grids model))

(defun set-digit-to-cell (grid row col digit-char)
  "Set the number to the grid at the given row and column to digit-char or delete if already there."
  (let* ((digit (position digit-char "0123456789"))
         (same-digit-p (= (aref grid row col) digit)))
    (setf (aref grid row col) (if same-digit-p 0 digit))))

(defun set-color-to-cell (grid row col color-char)
  "Set the grid cell's box color from the COLOR-CHAR A-I to the numbers 1-9."
  (let ((pos (position color-char "ABCDEFGHI")))
    (when pos
      (setf (aref grid row col) (1+ pos)))))

(defun select-cell (model row col)
  "Update the model when a cell is selected."
  (when (and (<= 0 row 8) (<= 0 col 8))
    (let ((current-item (current-item model)))
      (cond ((not current-item)
             (setf (current-cell model) (cons row col)))
            ((digit-char-p current-item)
             (set-digit-to-cell (puzzle-grid model) row col current-item))
            (t
             (set-color-to-cell (box-grid model) row col current-item))))
    (update-grids model)))

(defun select-item (model item-id)
  "Update the model when an item is selected (the characters 1-9 for digits or A-I for box colors)."
  (if (current-cell model)
      (destructuring-bind (row . col) (current-cell model)
        (if (digit-char-p item-id)
            (set-digit-to-cell (puzzle-grid model) row col item-id)
          (set-color-to-cell (box-grid model) row col item-id)))
    (setf (current-item model) item-id))
  (update-grids model))

(defun unselect-cell (model)
  "Undo the current cell selection."
  (setf (current-cell model) nil))

(defun unselect-item (model)
  "Undo the current item selection."
  (setf (current-item model) nil))

(defun unselect-all (model)
  "Reset the current cell and item."
  (unselect-cell model)
  (unselect-item model))

(defun bad-number-p (model row col)
  "Return T if the number on the Sudoku grid at (row, col) is not unique in its row/column/box."
  (member (cons row col) (grid-mistakes model) :test #'equal))

(defun bad-box-color-p (model row col)
  "Return T if the box color on the Sudoku grid at (row, col) is bad.
This is when it's not next to another cell with the same color, or if there are not nine of that color."
  (member (cons row col) (box-mistakes model) :test #'equal))

(defun erase-digit (model)
  "Erase the digit at the currently selected cell."
  (let ((curr (current-cell model)))
    (when curr
      (setf (aref (puzzle-grid model) (car curr) (cdr curr)) 0))))



;;; LispWorks-specific GUI code

(defconstant +cell-size+ 48 "The width/height of a Sudoku grid cell in pixels.")

(defconstant +font-size+ 31 "The font size for drawing numbers in cells.")

(defconstant +grid-size+ (* +cell-size+ 9) "The width/height of a Sudoku grid in pixels.")

(defconstant +selector-size+ (* +cell-size+ 18) "The width of the Selector Pane in pixels.")

(defvar *selector-chars* "123456789ABCDEFGHI" "The characters that can be selected for grid values/colors.")

(defvar *box-colors*
  (flet ((rgb (red green blue)
           "Return an RGB color based on values from 0-255."
           (color:make-rgb (float (/ red 255)) (float (/ green 255)) (float (/ blue 255)))))
    (make-array 9 :initial-contents (list (rgb 85 148 167)
                                          (rgb 141 107 33)
                                          (rgb 106 116 123)
                                          (rgb 193 159 55)
                                          (rgb 64 112 157)
                                          (rgb 148 51 36)
                                          (rgb 96 70 162)
                                          (rgb 141 76 145)
                                          (rgb 69 145 71))))
  "Box colors drawn in the background of each cell - these are similar to the colors in Microsoft Sudoku.")

(capi:define-interface solver-interface ()
  ((model :reader model :initform (make-instance 'model)))
  (:panes
   (clear-all-numbers-button capi:push-button
                             :text "Clear All Numbers"
                             :accepts-focus-p nil
                             :callback 'clear-all-numbers-callback)
   (reset-box-colors-button capi:push-button
                            :text "Reset Box Colors"
                            :accepts-focus-p nil
                            :callback 'reset-box-colors-callback)
   (selector-pane capi:output-pane
                  :mnemonic-title "Click or Press a key to select the number or box color to put in cells (0 erases)."
                  :title-adjust :center
                  :reader selector-pane
                  :accepts-focus-p nil
                  :display-callback 'display-selector-pane
                  :input-model '(((:press :button-1) click-selector-pane))
                  :draw-with-buffer t
                  :visible-min-height +cell-size+
                  :visible-max-height +cell-size+
                  :visible-min-width +selector-size+
                  :visible-max-width +selector-size+)
   (puzzle-pane capi:output-pane
                :mnemonic-title "Puzzle"
                :title-adjust :center
                :reader puzzle-pane
                :accepts-focus-p t
                :display-callback 'display-grid-pane
                :input-model '((:character char-input-callback)
                               ((:key :press) key-input-callback)
                               ((:press :button-1) click-puzzle-pane))
                :draw-with-buffer t
                :visible-min-height +grid-size+
                :visible-max-height +grid-size+
                :visible-min-width +grid-size+
                :visible-max-width +grid-size+)
   (solution-pane capi:output-pane
                  :mnemonic-title "First Solution Found"
                  :title-adjust :center
                  :reader solution-pane
                  :accepts-focus-p nil
                  :display-callback 'display-grid-pane
                  :draw-with-buffer t
                  :visible-min-height +grid-size+
                  :visible-max-height +grid-size+
                  :visible-min-width +grid-size+
                  :visible-max-width +grid-size+))
  (:layouts
   (button-row-layout capi:row-layout
                      '(clear-all-numbers-button reset-box-colors-button))
   (grid-row-layout capi:row-layout
                    '(puzzle-pane solution-pane))
   (column-layout capi:column-layout
                  '(button-row-layout selector-pane grid-row-layout)
                  :adjust :center))
  (:default-initargs
   :layout 'column-layout
   :title "Sudoku Solver"))

(defmethod initialize-instance :after ((interface solver-interface) &key)
  "Perform post-initialization settings for the interface."
  (setf (capi:capi-object-property (puzzle-pane interface) :grid) (puzzle-grid (model interface))
        (capi:capi-object-property (puzzle-pane interface) :mark-current-cell) t
        (capi:capi-object-property (solution-pane interface) :grid) (solution-grid (model interface))
        (capi:capi-object-property (solution-pane interface) :mark-current-cell) nil))

(defun invalidate-all (interface)
  "Cause all GUI output panes to be redrawn."
  (gp:invalidate-rectangle (selector-pane interface))
  (gp:invalidate-rectangle (puzzle-pane interface))
  (gp:invalidate-rectangle (solution-pane interface)))

(defun clear-all-numbers-callback (data interface)
  "Clear all numbers and redraw when the Clear All Numbers button is pressed."
  (declare (ignore data))
  (let ((model (model interface)))
    (clear-all-numbers model)
    (unselect-all model))
  (capi:set-pane-focus (puzzle-pane interface))
  (invalidate-all interface))

(defun reset-box-colors-callback (data interface)
  "Reset box colors back to the default and redraw when the Reset Box Colors button is pressed."
  (declare (ignore data))
  (let ((model (model interface)))
    (reset-all-boxes model)
    (unselect-all model))
  (capi:set-pane-focus (puzzle-pane interface))
  (invalidate-all interface))

(defun draw-bad-box-color (pane x y)
  "Draw a white X on a cell to indicate its box color has a problem."
  (let ((start-x (+ x 6))
        (end-x (+ x +cell-size+ -6))
        (start-y (+ y 6))
        (end-y (+ y +cell-size+ -6)))
    (gp:draw-lines pane (list start-x start-y end-x end-y start-x end-y end-x start-y)
                   :foreground :white :thickness 4)))

(defun draw-character (pane x y character color)
  "Draw a character inside a cell given the (X, Y) coordinate of its top left corner."
  (let ((font (gp:find-best-font pane (gp:make-font-description :size +font-size+))))
    (multiple-value-bind (left top right bottom) (gp:get-character-extent pane character font)
      (let ((baseline-x (+ x 24 (- (/ (+ left right) 2))))
            (baseline-y (+ y 24 (- (/ (+ bottom top) 2)))))
        (gp:draw-character pane character baseline-x baseline-y :foreground color :font font)))))

(defun draw-cell (pane row col)
  "Draw the cell at (row, col) on the given pane."
  (let* ((x (* col +cell-size+))
         (y (* row +cell-size+))
         (model (model (capi:element-interface pane)))
         (digit (aref (capi:capi-object-property pane :grid) row col))
         (box-color (svref *box-colors* (1- (aref (box-grid model) row col)))))
    (gp:draw-rectangle pane x y +cell-size+ +cell-size+ :foreground box-color :filled t)
    (when (bad-box-color-p model row col)
      (draw-bad-box-color pane x y))
    (unless (zerop digit)
      (draw-character pane x y (digit-char digit) (if (bad-number-p model row col) :red :black)))
    (gp:draw-rectangle pane x y +cell-size+ +cell-size+ :foreground :black :thickness 1)))

(defun display-grid-pane (pane x y width height)
  "Draw the given grid pane."
  (declare (ignore x y width height))
  (dotimes (row 9)
    (dotimes (col 9)
      (draw-cell pane row col)))
  (let ((curr (current-cell (model (capi:element-interface pane)))))
    (when (and curr (capi:capi-object-property pane :mark-current-cell))
      (gp:draw-rectangle pane (1+ (* (cdr curr) +cell-size+)) (1+ (* (car curr) +cell-size+))
                         (- +cell-size+ 2) (- +cell-size+ 2) :foreground :white :thickness 2))))

(defun display-selector-pane (pane x y width height)
  "Draw the selector pane for choosing a number or box color."
  (declare (ignore x y width height))
  (loop with curr = (current-item (model (capi:element-interface pane)))
        for char across *selector-chars*
        for i from 0
        for start-x = (* i +cell-size+)
        for box-color = (if (digit-char-p char) :darkolivegreen (svref *box-colors* (- i 9)))
        do
        (gp:draw-rectangle pane start-x 0 +cell-size+ +cell-size+ :foreground box-color :filled t)
        (draw-character pane start-x 0 char (if (eql curr char) :aliceblue :black))
        (gp:draw-rectangle pane start-x 0 +cell-size+ +cell-size+ :foreground :black :thickness 1)))

(defun char-input-callback (pane x y character)
  "Respond to character inputs (arrow keys don't get handled here)."
  (declare (ignore x y))
  (let ((upcase (char-upcase character))
        (model (model (capi:element-interface pane))))
    (cond ((find upcase *selector-chars*) (select-item model upcase))
          ((member upcase '(#\. #\0 #\delete)) (erase-digit model))
          (t (unselect-all model))))
  (invalidate-all (capi:element-interface pane)))

(defun key-input-callback (pane x y key)
  "Respond to arrow key presses."
  (declare (ignore x y))
  (flet ((update-current-cell (row-change col-change)
           (let* ((interface (capi:element-interface pane))
                  (model (model interface))
                  (curr (current-cell model)))
             (if curr
                 (let ((new-row (+ (car curr) row-change))
                       (new-col (+ (cdr curr) col-change)))
                   (when (and (<= 0 new-row 8) (<= 0 new-col 8))
                     (setf (current-cell model) (cons new-row new-col))))
               (setf (current-cell model) (cons 0 0)))
             (setf (current-item model) nil)
             (invalidate-all interface))))
  (case (sys:gesture-spec-data key)
    (:up (update-current-cell -1 0))
    (:down (update-current-cell 1 0))
    (:left (update-current-cell 0 -1))
    (:right (update-current-cell 0 1)))))

(defun click-puzzle-pane (pane x y)
  "Respond to mouse button 1 clicks on the puzzle pane."
  (let ((interface (capi:element-interface pane)))
    (when (and (< x +grid-size+) (< y +grid-size+))
      (select-cell (model interface) (floor y +cell-size+) (floor x +cell-size+))
      (invalidate-all interface))))

(defun click-selector-pane (pane x y)
  "Respond to mouse button 1 clicks on the selector pane."
  (let* ((interface (capi:element-interface pane))
         (model (model interface)))
    (when (and (< x +selector-size+) (< y +selector-size+))
      (let ((selected-item (char *selector-chars* (floor x +cell-size+))))
        (if (eql selected-item (current-item model))
            (unselect-item model)
          (select-item model selected-item))
        (invalidate-all interface)))))

(defun main ()
  (capi:display (make-instance 'solver-interface)))




;;; Testing code (*hardest20* and *hardest20-solutions* come from
;;; https://attractivechaos.github.io/plb/kudoku.html)

(defvar *hardest20*
  '("..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9"
    ".......12........3..23..4....18....5.6..7.8.......9.....85.....9...4.5..47...6..."
    ".2..5.7..4..1....68....3...2....8..3.4..2.5.....6...1...2.9.....9......57.4...9.."
    "........3..1..56...9..4..7......9.5.7.......8.5.4.2....8..2..9...35..1..6........"
    "12.3....435....1....4........54..2..6...7.........8.9...31..5.......9.7.....6...8"
    "1.......2.9.4...5...6...7...5.9.3.......7.......85..4.7.....6...3...9.8...2.....1"
    ".......39.....1..5..3.5.8....8.9...6.7...2...1..4.......9.8..5..2....6..4..7....."
    "12.3.....4.....3....3.5......42..5......8...9.6...5.7...15..2......9..6......7..8"
    "..3..6.8....1..2......7...4..9..8.6..3..4...1.7.2.....3....5.....5...6..98.....5."
    "1.......9..67...2..8....4......75.3...5..2....6.3......9....8..6...4...1..25...6."
    "..9...4...7.3...2.8...6...71..8....6....1..7.....56...3....5..1.4.....9...2...7.."
    "....9..5..1.....3...23..7....45...7.8.....2.......64...9..1.....8..6......54....7"
    "4...3.......6..8..........1....5..9..8....6...7.2........1.27..5.3....4.9........"
    "7.8...3.....2.1...5.........4.....263...8.......1...9..9.6....4....7.5..........."
    "3.7.4...........918........4.....7.....16.......25..........38..9....5...2.6....."
    "........8..3...4...9..2..6.....79.......612...6.5.2.7...8...5...1.....2.4.5.....3"
    ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."
    ".......12....35......6...7.7.....3.....4..8..1...........12.....8.....4..5....6.."
    "1.......2.9.4...5...6...7...5.3.4.......6........58.4...2...6...3...9.8.7.......1"
    ".....1.2.3...4.5.....6....7..2.....1.8..9..3.4.....8..5....2....9..3.4....67....."))

(defvar *hardest20-solutions*
  '("987654321246173985351928746128537694634892157795461832519286473472319568863745219"
    "839465712146782953752391486391824675564173829287659341628537194913248567475916238"
    "123456789457189236869273154271548693346921578985637412512394867698712345734865921"
    "562987413471235689398146275236819754714653928859472361187324596923568147645791832"
    "126395784359847162874621953985416237631972845247538691763184529418259376592763418"
    "174385962293467158586192734451923876928674315367851249719548623635219487842736591"
    "751846239892371465643259871238197546974562318165438927319684752527913684486725193"
    "125374896479618325683952714714269583532781649968435172891546237257893461346127958"
    "123456789457189236896372514249518367538647921671293845364925178715834692982761453"
    "123456789456789123789123456214975638375862914968314275591637842637248591842591367"
    "239187465675394128814562937123879546456213879798456312367945281541728693982631754"
    "743892156518647932962351748624589371879134265351276489496715823287963514135428697"
    "468931527751624839392578461134756298289413675675289314846192753513867942927345186"
    "728946315934251678516738249147593826369482157852167493293615784481379562675824931"
    "317849265245736891869512473456398712732164958981257634174925386693481527528673149"
    "621943758783615492594728361142879635357461289869532174238197546916354827475286913"
    "693784512487512936125963874932651487568247391741398625319475268856129743274836159"
    "673894512912735486845612973798261354526473891134589267469128735287356149351947628"
    "174835962293476158586192734957324816428961375361758249812547693635219487749683521"
    "869571324327849516145623987952368741681497235473215869514982673798136452236754198"))

(defun test-sudoku-hardest20 ()
  (loop for input in *hardest20*
        for expected in *hardest20-solutions*
        for actual = (grid-to-string (solve (string-to-grid input)))
        do (assert (string= expected actual))))

(defun test-irregular-boxes ()
  (let* ((sudoku-string "....2...12....96..53.....27.923761....71...3...69415.33......9.....6...57...3..1.")
         (box-string "111222233111122223441155233444555533644475593664777593666677799866877999888888899")
         (expected "948527361253719684531698427492376158687154239826941573315482796179263845764835912")
         (actual (grid-to-string (solve (string-to-grid sudoku-string) (string-to-grid box-string)))))
    (assert (string= expected actual))))

(defun test-sudoku-grid-mistakes ()
  (let* ((box-string "111222233111122223441155233444555533644475593664777593666677799866877999888888899")
         (box-grid (string-to-grid box-string))
         (good-string "....2...12....96..53.....27.923761....71...3...69415.33......9.....6...57...3..1.")
         (good-grid (string-to-grid good-string))
         (bad-string-1 "....2...12....96..53.....272923761....71...3...69415.33......9.....6...57...3..1.")
         (bad-grid-1 (string-to-grid bad-string-1))
         (bad-string-2 "....2...12....96..53.2...27.923761....71...3...69415.33......9.....6...57...3..1.")
         (bad-grid-2 (string-to-grid bad-string-2)))
    (assert (null (sudoku-grid-mistakes good-grid box-grid)))
    (assert (equal '((1 . 0) (3 . 0) (3 . 2)) (sudoku-grid-mistakes bad-grid-1 box-grid)))
    (assert (equal '((1 . 0) (2 . 3) (2 . 7)) (sudoku-grid-mistakes bad-grid-2 box-grid)))))

(defun test-box-grid-mistakes ()
  (assert (null (box-grid-mistakes *default-box-grid*)))
  (let ((good-string "111222233111122223441155233444555533644475593664777593666677799866877999888888899"))
    (assert (null (box-grid-mistakes (string-to-grid good-string)))))
  (let ((missing-a-one ".11222233111122223441155233444555533644475593664777593666677799866877999888888899"))
    (assert (equal '((0 . 0) (0 . 1) (0 . 2) (1 . 0) (1 . 1) (1 . 2) (1 . 3) (2 . 2) (2 . 3))
                   (box-grid-mistakes (string-to-grid missing-a-one)))))
  (let ((too-many-ones "111122233111122223441155233444555533644475593664777593666677799866877999888888899"))
    (assert (equal '((0 . 0) (0 . 1) (0 . 2) (0 . 3) (0 . 4) (0 . 5) (0 . 6)
                     (1 . 0) (1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5) (1 . 6) (1 . 7)
                     (2 . 2) (2 . 3) (2 . 6))
                   (box-grid-mistakes (string-to-grid too-many-ones)))))
  (let ((separate-1-and-8 "811222233111122223441155233444555533644475593664777593666677799866877999888818899"))
    (assert (equal '((0 . 0) (8 . 4))
                   (box-grid-mistakes (string-to-grid separate-1-and-8))))))

(defun run-all-tests ()
  (test-sudoku-hardest20)
  (test-irregular-boxes)
  (test-sudoku-grid-mistakes)
  (test-box-grid-mistakes))
