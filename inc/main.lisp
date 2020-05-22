;;;; @file main.lisp
;;; The "main" function of the program.
;;; Runs general gameplay process and calls all other functions for gameplay.
;;;
;;; Part of connect4.lisp
;;;



; Use ~a to print in "human readable" format.
(defun main()

  ; Reset and ensure game variables are empty.
  (reset-game)
  (ai-reset)

  ; Print headers to the screen.
  (format t "~%~%WELCOME TO...~%~%")
  (format t "  oooooooo8                                                               o8              o88   
o888     88   ooooooo   oo oooooo   oo oooooo    ooooooooo8   ooooooo   o888oo          o8888   
888         888     888  888   888   888   888  888oooooo8  888     888  888          o88 888   
888o     oo 888     888  888   888   888   888  888         888          888        o888oo888oo 
 888oooo88    88ooo88   o888o o888o o888o o888o   88oooo888   88ooo888    888o           o888o  ~%~%")
  (format t "~%5 |_|_|_|_|_|~%4 |_|_|_|_|_|~%3 |_|_|_|_|_|~%2 |_|o|x|_|x|~%1 |o|x|o|o|o|~%   1 2 3 4 5")  
  
  ; Ask the user if they want to play.
  (format t "~%~%Would you like to play?~%(1 = Yes 2 = No)~%> ")
  (setq play (read))

  ; If the user decides to play then ask for difficulty level.
  (cond                                                                                                                                    
    ((eq 1 play) (format t "~%What difficulty would you like to play on?~%0. Beginner~%1. Intermediate~%2. Hard~%3. Impossible~%> ") (setf difficulty (read)))
    ((eq 2 play) (return-from main (format t "~%Thank you for playing!"))))

  ; Set the first player randomly (0 to 1 [Inclusive])
  (setf first (random 2))

  ; Call functions depending on who's playing first.
  (cond 
    ((eq first 0) (format t "~%The computer has been randomly chosen to go first.~%") (gamePlay))
    ((eq first 1) (format t "~%You have been randomly chosen to go first.~%") (gamePlay)))

  (main) ; Call main recursively to play again.

  ) ; End of main function.


(defun addPlayerList(input)
  (let ((col input) (row 1) (player 1) (flag 0))
    (loop for move in game do
      (if (and (equal (nth 0 move) col) (>= (nth 1 move) 6))
        (and (setf flag 1) (gamePlay))
        (if (equal (nth 0 move) col)
          (setf row (+ 1 (nth 1 move))))))

    (if (equal 0 flag)
      (add-to-moves-list (list col row player)))))
      ; End of addPlayerList function.
                        
                  
(defun gamePlay()

  (cond
    ((and (> (length game) 0) (eq T (winCheck game)))
      (format t "~%GAME RESULTS:~%")
      (display)
      (return-from gamePlay (format t "~%~%Thank you for playing!~%~%~%"))))

  ; Make computer move first if computer moves first.
  (if (eq first 0)
    (ai))

  (display)

  (cond
    ((eq T (winCheck game))
      (format t "~%GAME RESULTS:~%")
      (display)
      (return-from gamePlay (format t "~%~%Thank you for playing!~%~%~%"))))

  (format t "~%~%Choose a column to make your move (1-5): ~%> ")        
  (setq input (read))
  (addPlayerList input)

  ; Make computer move second if computer moves second.
  (if (eq first 1)
    (ai))

  (cond
    ((and (> (length game) 0) (eq T (winCheck game)))
      (format t "~%GAME RESULTS:~%")
      (display)
      (return-from gamePlay (format t "~%~%Thank you for playing!~%~%~%"))))

  (gamePlay))
  ; End of gamePlay function.


; Call the actual program.
(main)