# sudoku-solver
This is a program to help solve puzzles in Microsoft Sudoku. Eventually
this will have a GUI to be more user friendly. It's written in LispWorks 7.1.1
64 bit on Windows 10, not tested yet on other implementations.

There's a lot of Sudoku solvers out there. What's special about this one?
- This supports solving Sudoku puzzles with irregular shaped boxes.
  Regular Sudoku requires the digits 1-9 on each row, column, and in nine
  3x3 boxes, but Microsoft Sudoku has a variant with irregular shaped
  boxes instead of 3x3.

Things other Sudoku solvers support, but aren't supported here:
- This only supports 9x9 grids, but other solvers support different sizes.
- This stops searching after finding any valid solution. Other solvers can
  continue searching to check if there are multiple solutions.
- This uses Knuth's Dancing Links / Algorithm X to solve Sudoku as an exact
  cover problem, but it isn't trying to be the fastest solver out there.

# Usage
1. Solving a regular Sudoku puzzle
```lisp
(solve (string-to-grid "..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9"))
```
Will return the solution as a 2D array like:
```lisp
#2A((9 8 7 6 5 4 3 2 1)
    (2 4 6 1 7 3 9 8 5)
    (3 5 1 9 2 8 7 4 6)
    (1 2 8 5 3 7 6 9 4)
    (6 3 4 8 9 2 1 5 7)
    (7 9 5 4 6 1 8 3 2)
    (5 1 9 2 8 6 4 7 3)
    (4 7 2 3 1 9 5 6 8)
    (8 6 3 7 4 5 2 1 9))
```

2. Solving a Sudoku puzzle with irregular shaped boxes/regions:
```lisp
(let ((sudoku-string "....2...12....96..53.....27.923761....71...3...69415.33......9.....6...57...3..1.")
      (box-string "111222233111122223441155233444555533644475593664777593666677799866877999888888899"))
  (solve (string-to-grid sudoku-string) (string-to-grid box-string)))
```
Will return:
```lisp
#2A((9 4 8 5 2 7 3 6 1)
    (2 5 3 7 1 9 6 8 4)
    (5 3 1 6 9 8 4 2 7)
    (4 9 2 3 7 6 1 5 8)
    (6 8 7 1 5 4 2 3 9)
    (8 2 6 9 4 1 5 7 3)
    (3 1 5 4 8 2 7 9 6)
    (1 7 9 2 6 3 8 4 5)
    (7 6 4 8 3 5 9 1 2))
```

The code uses a string representation of a Sudoku grid, such as:
```lisp
"..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9"
```
or
```lisp
". . . | . . . | . . .
 . . . | . . 3 | . 8 5
 . . 1 | . 2 . | . . .
 ------+-------+------
 . . . | 5 . 7 | . . .
 . . 4 | . . . | 1 . .
 . 9 . | . . . | . . .
 ------+-------+------
 5 . . | . . . | . 7 3
 . . 2 | . 1 . | . . .
 . . . | . 4 . | . . 9"
 ```

The rules for the string representation are:
1. 0 or . represent empty cells, 1-9 are the digits.
2. Every other character is ignored.
3. The digits/empty spaces are added row by row from left to right and
   top to bottom.

The string representation to describe the boxes are the same but
with a few extra rules to make sure it's valid:
1. There must be no empty cells.
2. Every digit must be next to at least one cell with the same digit
  so that each box/region is one continuous blob.
3. The digits 1-9 must each be used nine times.

For example, the box description grid in a regular Sudoku puzzle are:
```lisp
"1 1 1 | 2 2 2 | 3 3 3
 1 1 1 | 2 2 2 | 3 3 3
 1 1 1 | 2 2 2 | 3 3 3
 ------+-------+------
 4 4 4 | 5 5 5 | 6 6 6
 4 4 4 | 5 5 5 | 6 6 6
 4 4 4 | 5 5 5 | 6 6 6
 ------+-------+------
 7 7 7 | 8 8 8 | 9 9 9
 7 7 7 | 8 8 8 | 9 9 9
 7 7 7 | 8 8 8 | 9 9 9"
```

And here's an example of a box grid with irregular boxes:
```lisp
"1 1 1 | 2 2 2 | 2 3 3
 1 1 1 | 1 2 2 | 2 2 3
 4 4 1 | 1 5 5 | 2 3 3
 ------+-------+------
 4 4 4 | 5 5 5 | 5 3 3
 6 4 4 | 4 7 5 | 5 9 3
 6 6 4 | 7 7 7 | 5 9 3
 ------+-------+------
 6 6 6 | 6 7 7 | 7 9 9
 8 6 6 | 8 7 7 | 9 9 9
 8 8 8 | 8 8 8 | 8 9 9"
```

