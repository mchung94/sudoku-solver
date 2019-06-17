;;;; Sudoku Solver - written with LispWorks 7.1.1 64-bit on Windows 10

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

(defun box-grid-mistakes (box-grid)
  "Return a list of (row . column) conses for bad cell values in a box grid.
Box grids use the digits 1-9 to indicate the different boxes/regions that must
contain the digits 1-9 (each row, column, and box must contain 1-9 in the
Sudoku solution). In a regular Sudoku puzzle, the boxes are nine 3x3 regions,
but Microsoft Sudoku supports irregular-shaped boxes. Cell values are bad when
they're empty, not next to another cell with the same digit, or if there are not
exactly nine of the same digit."
  (labels ((neighbors (row col)
             (loop for r from (1- row) to (1+ row)
                   when (<= 0 r 8)
                   nconc (loop for c from (1- col) to (1+ col)
                               when (and (<= 0 c 8)
                                         (not (and (= c col) (= r row))))
                               collect (cons r c))))
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
ignore all other characters."
  (loop with grid = (make-array '(9 9) :element-type 'grid-value :initial-element 0)
        for ch across (remove-if (complement #'digit-char-p) (substitute #\0 #\. string))
        for index from 0
        do (setf (row-major-aref grid index) (position ch "0123456789"))
        finally (return grid)))

(defun grid-to-string (grid &optional add-whitespace)
  "Convert a grid into a string. Periods are empty cells, 1-9 are digits.
Calling (STRING-TO-GRID (GRID-TO-STRING grid)) will create a copy of a grid.
If ADD-WHITESPACE is true then it will return a string with spaces/newlines to
be easier to see."
  (format nil (if add-whitespace "~{~{~C~^ ~}~^~%~}" "~{~{~C~}~}")
          (loop for row from 0 below 9
                collect (loop for col from 0 below 9
                              collect (char ".123456789" (aref grid row col))))))

(defvar *default-boxes*
  (string-to-grid "111222333111222333111222333444555666444555666444555666777888999777888999777888999")
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

(defun solve (sudoku-grid &optional (box-grid *default-boxes*))
  "Solve the SUDOKU-GRID puzzle where the boxes are defined by BOX-GRID.
Return a single grid solution."
  (let* ((grid (string-to-grid (grid-to-string sudoku-grid)))
         (root (make-empty-matrix *constraint-names*))
         (columns (coerce (matrix-columns root) 'vector)))
    (labels ((set-digit (&key row col digit box)
               (declare (ignore box))
               (setf (aref grid row col) digit))
             (row-names (data)
               (loop for d = data then (right d)
                     repeat 4
                     append (name (column d))))
             (add-data-to-solution (data)
               (apply #'set-digit (row-names data))))
      (dotimes (row 9)
        (dotimes (col 9)
          (loop with box = (aref box-grid row col)
                for digit from 1 to 9
                do (add-row (get-columns columns row col box digit)))))
      (dotimes (row 9)
        (dotimes (col 9)
          (let ((digit (aref sudoku-grid row col))
                (box (aref box-grid row col)))
            (unless (zerop digit)
              (mapc #'cover-column (get-columns columns row col box digit))))))
      (mapc #'add-data-to-solution (dlx-search root))
      grid)))



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
        do (assert (string= expected (grid-to-string (solve (string-to-grid input)))))))

(defun test-irregular-boxes ()
  (let ((sudoku-string "....2...12....96..53.....27.923761....71...3...69415.33......9.....6...57...3..1.")
        (box-string "111222233111122223441155233444555533644475593664777593666677799866877999888888899"))
    (assert (string= (grid-to-string (solve (string-to-grid sudoku-string) (string-to-grid box-string)))
                     "948527361253719684531698427492376158687154239826941573315482796179263845764835912"))))

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
  (assert (null (box-grid-mistakes *default-boxes*)))
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