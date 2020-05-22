;;;; @file helper.lisp
;;; "Helper" functions that could be used commonly among program dependencies.
;;;
;;; Part of connect4.lisp
;;;

(defun add-to-moves-list(move)
  "Simple helper function that adds a move to the end of the game local variable."
  (setf game (append game (list move))))
  ;; End of 'add-to-moves-list' function.


(defun check-moves()
  "Checks the validity of *every* move in the game global variable at each call.
  1. Checks that moves alternate (player, AI, player, AI) or (AI, player, AI, player).
  2. Checks that every column is <= 5 and row move <= 5.
  3. Checks that each row move is greater than the last move made in the corresponding column.
  4. Checks that each move has a valid player assignment (0 for AI, 1 for player).
  5. Checks that the total number of moves is <= the maximum possible number of moves (64 on an 7x6 gameboard)."

  (let ((message "~%") (i 1))
    (loop for k from 0 to (length game) do
      ;; Check that player and AI moves alternate.
      (if (and (not (equal k 0)) (equal (nth 2 (nth k game)) (nth 2 (nth (- k 1) game))))
        (setf message (concatenate 'string message "Move #" (write-to-string (+ k 1)) " is invalid. Player " (write-to-string (nth 2 (nth k game))) " has made 2 simultaneous moves.~%")))

      ;; Check that each row move is greater than the last.
      (loop for n from k to (length game) do
        (if (not (equal k n))
          (if (and (equal (nth 0 (nth k game))(nth 0 (nth n game))) (<= (nth 1 (nth n game)) (nth 1 (nth k game))))
            (setf message (concatenate 'string message "Move #" (write-to-string (+ n 1)) " has an invalid row value (It's row value is less than or equal to a row value in the same column that came before it.).~%")))
        )
    ))

    ;; Check that the number of moves is less than or equal to the maximum possible number of moves.
    (if (>= (length game) 25)
      (setf message (concatenate 'string message "The number of total moves == 25. On an 5x5 board any more than this would be illegal. The game should be over by now.~%")))
    
    (loop for move in game do
      (cond
        ;; Check that a column move is <= 5
        ((or (> (nth 0 move) 5) (< (nth 0 move) 1)) (setf message (concatenate 'string message "Move #" (write-to-string i) " contained a column with a value > 5.~%")))
        ;; Check that a row move is <= 5
        ((or (> (nth 1 move) 5) (< (nth 1 move) 1)) (setf message (concatenate 'string message "Move #" (write-to-string i) " contained a row with a value > 5.~%")))
        ;; Check that a player value == 0 || 1 || 2
        ((or (> (nth 2 move) 2) (< (nth 2 move) 0)) (setf message (concatenate 'string message "Move #" (write-to-string i) " has an invalid player assinged to its move.~%")))
      )
      (setf i (+ i 1)))

    (if (equal message "~%")
      t
      (and (format t message) nil))))
      ; End of 'check-moves' function.


(defun print-game()
  "Prints each game move."
  (let ((i 0))
    (loop for move in game do
      (setf i (+ i 1))
      (format t "Move ~D# ~S~%" i move))))
      ; End of 'print-game' function.


(defun pick-random-from-list(x)
  "@return a random item from the given list x."

  ;; Uncomment to debug
  ;(print x)

  (if (and x (listp x))
    (nth (random (- (+ (length x) 1) 1)) x)
    (print "Please pass this fuction a list!")))
    ; End of 'pick-random-from-list' function.


(defun available-move(desired-move)
  "Check the game variable to see if a move is available.
   desired-move must at least be a list in the form (col row)...
   however, (col row player) [an element from the game var] is
   acceptable as well."

  (let ((col (nth 0 desired-move)) (row (nth 1 desired-move)))
    (loop for move in game do
      (if (and (eq col (nth 0 move)) (eq row (nth 1 move)))
        (return-from available-move nil))))

  T)
  ; End of available-move function


(defun simulate-moves()
  "A function that simulates moves by adding them to the game list for debugging only!"
  (ai-reset)
  (loop for i from 1 to 35 do
    (let ((col  (+ (random 7) 1)) (row  1) (player 0) (add T))
      
      ; Set the row
      (loop for i from 0 to (- (length game) 1) do
        (if (eq col (nth 0 (nth i game)))
          (setf row (+ row 1))))

      (if (> row 5)
        (return))

      ; Set a player
      (if (eq (mod i 2) 0)
        (setf player 0)
        (setf player 1))

      ; Make random move, if the move isn't in the game list.
      (loop for move in game do
        (if (and (eq (nth 0 move) col) (eq (nth 1 move) row))
          (setf add nil)
          (setf add T)))

      (if (eq T add)
        (setf game (append game (list (list col row player)))))))

  (display) ; Display the board before we exit.
  )
  ; End of 'pre-populate' function.
