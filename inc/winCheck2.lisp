
(defun winCheck(gamelist)
	; Return if the list is empty
	(if (eq nil gamelist)
		(return-from winCheck nil))

	(set-move-positions)

	nil

	(if (eq T (check-win 0))
		(and (printWinner 0) (return-from winCheck T)))

	(if (eq T (check-win 1))
		(and (printWinner 1) (return-from winCheck T))))

(defun check-win(player)
	(let ((win nil) (winner nil) (count 0))

		; Check row/col combination
		(loop for i from 0 to (- (length move-positions) 1) by 5 do

			; Check each row
			(loop for n from 0 to 5 do

				; Horizontal checks
				(setf count 0)
				(cond ((and (eq (nth (+ i n) move-positions) player) (< n 5))
					(if
						(eq (nth (+ i n 1) move-positions) player) (setf count (+ 1 count)))
					(if
						(eq (nth (+ i n 2) move-positions) player) (setf count (+ 1 count)))
					(if	
						(eq (nth (+ i n 3) move-positions) player) (setf count (+ 1 count)))))

				(if (>= count 3)
					(and (setf win T) (setf winner player)))

				; Vertical checks
				(setf count 0)
				(cond ((and (eq (nth (+ i n) move-positions) player) (< (+ i n) 21))
					(if
						(eq (nth (+ i n  7) move-positions) player) (setf count (+ 1 count)))
					(if
						(eq (nth (+ i n 14) move-positions) player) (setf count (+ 1 count)))
					(if
						(eq (nth (+ i n 21) move-positions) player) (setf count (+ 1 count)))))

				(if (>= count 3)
					(and (setf win T) (setf winner player)))

				; Diagonal right check
				(setf count 0)
				(cond ((and (eq (nth (+ i n) move-positions) player) (< (+ i n) 17))
					(if
						(eq (nth (+ i n  8) move-positions) player) (setf count (+ 1 count)))
					(if
						(eq (nth (+ i n 16) move-positions) player) (setf count (+ 1 count)))
					(if
						(eq (nth (+ i n 24) move-positions) player) (setf count (+ 1 count)))))

				(if (>= count 3)
					(and (setf win T) (setf winner player)))

				; Diagonal left check
				(setf count 0)
				(cond ((and (eq (nth (+ i n) move-positions) player) (< (+ i n) 23))
					(if
						(eq (nth (+ i n  6) move-positions) player) (setf count (+ 1 count)))
					(if
						(eq (nth (+ i n 12) move-positions) player) (setf count (+ 1 count)))
					(if
						(eq (nth (+ i n 18) move-positions) player) (setf count (+ 1 count)))))

				(if (>= count 3)
					(and (setf win T) (setf winner player)))))
	win))

(defun printWinner(winner) 
  (if (eq winner 1)
    (format t "~%~%##    ##  #######  ##     ##    ##      ## #### ##    ## #### 
 ##  ##  ##     ## ##     ##    ##  ##  ##  ##  ###   ## #### 
  ####   ##     ## ##     ##    ##  ##  ##  ##  ####  ## #### 
   ##    ##     ## ##     ##    ##  ##  ##  ##  ## ## ##  ##  
   ##    ##     ## ##     ##    ##  ##  ##  ##  ##  ####      
   ##    ##     ## ##     ##    ##  ##  ##  ##  ##   ### #### 
   ##     #######   #######      ###  ###  #### ##    ## ####~%~%")
    (format t "~%~%##    ##  #######  ##     ##    ##        #######   #######   ######  ######## #### 
 ##  ##  ##     ## ##     ##    ##       ##     ## ##     ## ##    ## ##       #### 
  ####   ##     ## ##     ##    ##       ##     ## ##     ## ##       ##       #### 
   ##    ##     ## ##     ##    ##       ##     ## ##     ##  ######  ######    ##  
   ##    ##     ## ##     ##    ##       ##     ## ##     ##       ## ##            
   ##    ##     ## ##     ##    ##       ##     ## ##     ## ##    ## ##       #### 
   ##     #######   #######     ########  #######   #######   ######  ######## ####~%~%"))
   T)


