(in-package #:sudoku-solver)

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
