;;;; @file ai.lisp
;;; The "artifical intelligence" engine portion of the program.
;;; Computes and makes computer moves.
;;;
;;; Part of connect4.lisp

;(setf difficulty 0) ; For easy difficulty
;(setf difficulty 1) ; For intermediate difficulty
;(setf difficulty 2) ; For hard difficulty
;(setf difficulty 3) ; For insane difficulty

;; Variable that holds a table of positions made and emulates the board. => ~ for unmade move, 0 of AI Move, 1 for player move.
(if (not (boundp 'move-positions))
  (defvar move-positions '()))

(loop for i from 0 to 41 do
  (setf move-positions (append '(-1) move-positions)))

;; -------------------------------------------- END OF SAMPLE DATA FOR TESTING -------------------------------------------

;; Variable that check how many times random column generator ran. Uncomment line #48 to debug.
(defvar num-calcs 0)


;; AI Move Function "Wrapper."
(defun ai()
	"Function that determines the function to handle the AI board move based on the difficulty option the user has chosen.

  @global game - A list of the currently made moves and will be used to append player, as well as AI moves. Also determines
  the AI move to be made. Ex: ((1 1 0) (1 2 1) (2 1 0) (2 2 1)). The first variable in the sub-list is the column of that move,
  the second is the row, and the third represents the player.

  Ex: game(move-1(column, row, player)) is a game move, where player == 0 for AI, or 1 for player, 1 <= row <= 6, 1 <= col <= 7).

  @global difficulty - The gameplay difficulty:
  0 = Easy
  1 = intermediate
  2 = Hard
  3 = Insane"

  ;; Make sure game has been set before the function was called.
  (if (not (boundp 'game))
    (defvar game '()))

  ;; Make sure difficulty has been set before the function was called.
  (if (not (boundp 'difficulty))
    (defvar difficulty 0))

  ;; Save last move made by AI.
  (if (not (boundp 'ai-last-move))
    (defvar ai-last-move 0))

  ;; Call the appropriate difficulty function and check the validity of moves...
	
  (if t ; BYPASS MOVE CHECKING FOR DEBUGGING...
  ;(if (equal (check-moves) t)
    (cond
  		((equal difficulty 0) (ai-move-easy))   ; "Easy" mode was chosen, call easy function.
  		((equal difficulty 1) (ai-move-intm))   ; "Intermediate mode was chosen, call intermediate function.
  		((equal difficulty 2) (ai-move-hard))   ; "Hard" mode was chosen, call hard function.
      ((equal difficulty 3) (ai-move-insane)) ; "Insane" mode was chosen, call insane function.
      (T (ai-move-easy))))                ; No difficulty was set, use "easy mode."

      ;; Uncomment to debug.
      ; (print num-calcs))
      ) ; End of 'ai-move' function.


;; Actual AI Move Functions...
(defun ai-move-easy(&optional col-list)
  "Function that determines an 'easy' AI move.

  Selects a legal move at random and adds a move to the global variable 'game'

  @param col      - A random int between 1 and 7, picks the column.
  @param row      - An int that represents the row for the move (incremented by the sum of pieces in each column).
  @param player   - Always 0 here, as AI Player == 0.
  @param flag     - Don't add the element to the list if it's invalid! (fixes recursion problem).
  @param col-list - A list of the available columns with valid moves. Helps prevent stack overflow for random variable."

  ;; Increment the number of iterations (for debugging)
  ;(setf num-calcs (+ num-calcs 1))

  ;; If not passed the col-list, set all columns valid.
  (if (equal (length col-list) 0)
    (setf col-list '(1 2 3 4 5)))


  (let ((col (pick-random-from-list col-list)) (row 1) (player 0) (flag 0))   ; Set local variables. Call 'pick-random-from-list' to get the random column number.
    (loop for move in game do                                                 ; Loop through each move in the game global variable.
      (if (and (equal (nth 0 move) col) (>= (nth 1 move) 5))                  ; If the column in the game list move == the random column move chosen and it's row >= 5...
        (and (setf flag 1) (ai-move-easy (remove col col-list)))              ; Then call the function again, and try for a new random column, as this column is maxed out.
        (if (equal (nth 0 move) col)                                          ; Otherwise, if the column in the game move == the random column move, and row < 5...
          (setf row (+ 1 (nth 1 move))))))                                    ; Then row = the move's row + 1.
   
    (if (equal 0 flag)
      (and (setf ai-last-move (convert-move-to-num (list col row player))) (add-to-moves-list (list col row player))))))  ; Call the helper function to add move to end of list.
      ; End of 'ai-move-easy' function.


(defun ai-move-intm()
  "Function that determines an 'intermediate' AI move.

  Selects a legal move at random and adds a move to the global variable 'game',
  attempts to get 4 in a row as quickly as possible.

  @param concurrent-moves - A list of 2 or moves in a row made by the AI player..."

  ; Set the move-positions variable before each move.
  (set-move-positions)
  
  (let ((concurrent-moves '()) (canidate-move nil))
    ; Not enough moves in the game to try to get pieces in a row,
    ; So just call "easy" function for random move.
    (if (or (eq (length game) 0))
      (return-from ai-move-intm (ai-move-easy)))

    ; Determine where to move, based on previous moves made by the AI
    ; Remember the board has "41" slots here since iteration begins at 0.
    (loop for i from 0 to (- (length move-positions) 1) by 5 do

      (loop for n from 0 to 5 do
        ; Check horizontal to the right
        (if (and (eq (nth (+ i n) move-positions) 0) (not (eq n 5)))
          (if (eq (nth (+ i n 1) move-positions) -1)

          ; Make sure there's a piece under this one...
          (if (> i 5)
            (if (eq T (check-below (+ n i 1)))
              (setf concurrent-moves (append concurrent-moves (list (+ n i 1)))))
            (setf concurrent-moves (append concurrent-moves (list (+ n i 1)))))))

        ; Check horizontal to the left
        (if (and (eq (nth (+ i n) move-positions) 0) (not (< n 1)))
          (if (eq (nth (- (+ i n) 1) move-positions) -1)

            ; Make sure there's a piece under this one...
            (if (> i 5)
              (if (eq T (check-below (- (+ n i) 1)))
                (setf concurrent-moves (append concurrent-moves (list (- (+ n i) 1)))))
              (setf concurrent-moves (append concurrent-moves (list (- (+ n i) 1)))))))

        ; Check vertical up
        (if (eq (nth (+ i n) move-positions) 0)
          (if (eq (nth (+ i n 5) move-positions) -1)
            (setf concurrent-moves (append concurrent-moves (list (+ i n 5))))))

        ; Check diagonal right
        (if (and (not (eq n 5)) (> i 5))
          (if (eq (nth (+ i n) move-positions) 0)
            (if (eq (nth (+ i n 6) move-positions) -1)

              ; Make sure there's a piece under this one...
              (if (eq T (check-below (+ i n 6)))
                (setf concurrent-moves (append concurrent-moves (list (+ i n 6))))))))

        ; Check diagonal left
        (if (and (not (eq n 0)) (> i 5))
          (if (eq (nth (+ i n) move-positions) 0)
            (if (eq (nth (+ i n 5) move-positions) -1)

              ; Make sure there's a piece under this one...
              (if (eq T (check-below (+ i n 5)))
                (setf concurrent-moves (append concurrent-moves (list (+ i n 5))))))))

      )) ; End of inner and outer loop


  ; Pick a random move from the concurrent-moves list...
  (if (and (listp concurrent-moves) (> (length concurrent-moves) 0))

    ; Pick closest to last move from concurrent moves list and add it to the game variable.
    ; This needs work, computer is semi-stupid at this point.
    ; @todo:
    ; Make the computer realize that if there's only 2 rows above left and no-streak,
    ; then you cannot possibly connect 4.
    (and (setf canidate-move (pick-closest-to-last concurrent-moves))
         (add-to-moves-list (convert-num-to-move canidate-move))
         (setf ai-last-move canidate-move))

    (ai-move-easy)))) ; No concurrent move, make a random move.
    ; End of ai-move-intm function.


(defun ai-move-hard()
  "Function that determines an 'easy' AI move.

  Selects a legal move at using algorithm and adds a move to the global variable 'game',
  attempts to get 4 in a row as qucikly as possible,
  and attempts to block potential user winds."

  ; Not enough moves to make a block, so just make a random move by calling ai-move-easy.
  (if (eq (length game) 0)
    (return-from ai-move-hard (ai-move-easy)))

  ; Set the move-positions variable before each move.
  (set-move-positions)

  (let
    ((potential-moves '())
    (sum 0))

    (loop for i from 0 to (- (length move-positions) 1) by 5 do

        ; Horizontal Blocks:
        ; Check that player hasn't made cols 1, 2, and 3 in a row.
        (setf sum 0)
        (loop for n from 0 to 2 do
          (if (eq (nth (+ i n) move-positions) 1)
            (setf sum (+ sum 1))))

        (if (>= sum 3)
          (setf potential-moves (append potential-moves (list (+ i 3)))))

        ; Check that player hasn't made cols 1, 2, and 3 in a row.
        (setf sum 0)
        (loop for n from 1 to 3 do
          (if (eq (nth (+ i n) move-positions) 1)
            (setf sum (+ sum 1))))

        (if (>= sum 3)
          (setf potential-moves (append potential-moves (list (+ i 0) (+ i 4)))))

        ; Check that player hasn't made cols 2, 3, and 4 in a row.
        (setf sum 0)
        (loop for n from 2 to 4 do
          (if (eq (nth (+ i n) move-positions) 1)
            (setf sum (+ sum 1))))

        (if (>= sum 3)
          (setf potential-moves (append potential-moves (list (+ i 1) (+ i 5)))))

        ; Check that player hasn't made cols 3, 4, and 5 in a row.
        (setf sum 0)
        (loop for n from 3 to 5 do
          (if (eq (nth (+ i n) move-positions) 1)
            (setf sum (+ sum 1))))

        (if (>= sum 3)
          (setf potential-moves (append potential-moves (list (+ i 2) (+ i 6)))))

        ; Check that player hasn't made cols 4, 5, and 6 in a row.
        (setf sum 0)
        (loop for n from 4 to 6 do
          (if (eq (nth (+ i n) move-positions) 1)
            (setf sum (+ sum 1))))

        (if (>= sum 3)
          (setf potential-moves (append potential-moves (list (+ i 3)))))


        ; Vertical Blocks
        (loop for n from 0 to 5 do
          (if (and (eq (nth (+ i n) move-positions) 1) (eq (nth (+ i n 7) move-positions) 1) (eq (nth (+ i n 14) move-positions) 1) (< (+ i n) 20))
            (setf potential-moves (append potential-moves (list (+ i n 21))))))

        ; Diagonal Right Blocks
        (loop for n from 0 to 6 do
          (if (and (eq (nth (+ i n) move-positions) 1) (eq (nth (+ i n 8) move-positions) 1) (eq (nth (+ i n 16) move-positions) 1) (< (+ i n) 17))
            (setf potential-moves (append potential-moves (list (+ i n 24))))))

        ; Diagonal Left Blocks
        (loop for n from 0 to 6 do
          (if (and (eq (nth (+ i n) move-positions) 1) (eq (nth (+ i n 6) move-positions) 1) (eq (nth (+ i n 12) move-positions) 1) (> (+ i n) 21))
            (setf potential-moves (append potential-moves (list (+ i n 18))))))

    ) ; End of outer and inner loops

    
    (loop for this-move in potential-moves do
      ; Remove the move from the potential moves list if the move has already been made.
      (if (not (available-move (convert-num-to-move this-move)))
        (setf potential-moves (remove this-move potential-moves :test #'equal)))

      ; Remove from the list if it's an invalid move.
      (if (eq nil (check-below this-move))
        (setf potential-moves (remove this-move potential-moves :test #'equal))))

    (if (< (length potential-moves) 1)
      (ai-move-intm) ; No blocks needed call ai-move-intm
      (add-to-moves-list (convert-num-to-move (pick-random-from-list potential-moves)))))) ; Make the block move.

      ; End of ai-move-hard function.


(defun ai-move-insane()
  "You just can't win :)"
  (ai-move-intm) ; No time to work on this, call hard.
)




;; -------------------------------------------------- AI SPECIFIC HELPERS ------------------------------------------------

(defun pre-populate()
  "A function that pre-populates the game list for debugging only!"
  (setf game '())

  (setf move-positions '())
  (loop for i from 0 to 41 do
    (setf move-positions (append '(-1) move-positions)))

  (loop for i from 1 to 35 do
    (ai))
    (display))
    ; End of 'pre-populate' function.

(defun ai-reset()
  "AI Helper function to reset vars."
  (setf game '())

  (setf move-positions '())
  (loop for i from 0 to 41 do
    (setf move-positions (append '(-1) move-positions))))
    ; End of 'ai-reset' function.


(defun set-move-positions()
  "Iterates through the game global and re-maps the move-position list."
  (loop for i from 0 to (- (length game) 1) do
    (setf (nth (+ (* 7 (- (nth 1 (nth i game)) 1)) (- (nth 0 (nth i game)) 1)) move-positions) (nth 2 (nth i game)))))
    ; End of 'set-move-positions' function.

(defun print-move-pos()
  "Prints the move-position list."
  (loop for move in move-positions do
    (print move)))
    ; End of 'print-move-pos' function.

(defun convert-num-to-move(move-num)
  "Converts a *move-position* move to its respective (col, row, player) move for
   insertion into the game global."
  (let ((col 0) (row 0))
    (setf row (+ (floor (/ move-num 7)) 1))
    (setf col (+ (mod move-num 7) 1))
    (list col row 0)))
    ; End of 'convert-num-to-move' function.

(defun convert-move-to-num(move)
  "Converts a (col, row, player) move to a *move-position* move for
   insertion into the move-position global."
  (+ (- (nth 0 move) 1) (* 7 (- (nth 1 move) 1))))

(defun check-below(num)
  "Checks the row below the num-th move-position to see if there's a piece below it.
   @param num - A *move-position* representation of a move."
  (if (> num 6) 
    (if (eq (nth (- num 7) move-positions) -1)
    nil
    T)
    T))
    ; End of 'check-below' function.

(defun pick-closest-to-last(concurrent-moves)
  "Picks the move closest to the last move."
  (let ((min 41) (move-num 0))
    (loop for i from 0 to (- (length concurrent-moves) 1) do
      (if (<= (abs (- (nth i concurrent-moves) ai-last-move)) min)
        (and (setf min (abs (- (nth i concurrent-moves) ai-last-move))) (setf move-num (nth i concurrent-moves)))))
    
    move-num))

