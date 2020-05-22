;;;-----------WINCHECKING ALGS------------------------------------
;;;Main win checking function.
;;;---------------------------------------------------------------

;;;Main win checking function.


(defun winCheck (game)
  (if (eq game nil)
    (return-from winCheck nil))

  (setf *compMoves* nil)
  (setf *playerMoves* nil)
  (setf *oponent* (whosWinCheck game))
  (setf *win* 0)
  (listSplit game)
    (cond ((equal *oponent* 'PLAYER) (mainWinCheck (reverse *playerMoves*)))
          ((equal *oponent* 'COMPUTER) (mainWinCheck (reverse *compMoves*)))))
                                  


;;;-----------------------------------------------
;;;Recurrsive definition to determine owner of move.
 

(defun listSplit (moveList)
  (setf testlist (CAR moveList))
  (cond ((null testlist) nil)
        ((equal (CDDR testlist) '(0))
         (and (push testlist *compMoves*) (listSplit (rest moveList)))) 
        ((equal (CDDR testlist) '(1))
         (and (push testlist *playerMoves*) (listSplit (rest moveList))))))



;;;-----------------------------------------------
;;;Determine who's win to check for.


(defun whosWinCheck (moveList)
  (cond ((equal (CADDR (CAR moveList)) '1) 'PLAYER)
        (t 'COMPUTER))) 


;;;------------------------------------------------
;;;Win checking functions call eachother.

;;;Calls first win checking function.
(defun mainWinCheck (oponentList)
  (cond ((winDiagonal_Down oponentList) t)))  


;;;Passes first arguments to winDiagonalDown. 
(defun winDiagonal_Down (list) 
    (setf firstElem (CAR list))
    (setf firstA (CAR firstElem))
    (setf firstB (CADR firstElem)) 
  (cond ((winDiagonalDown firstA firstB (rest list)) 
         (printWinner) T)  
        (t (winDiagonal_Up list))))  


;;;Check for Downward sloping win.	     
(defun winDiagonalDown (prevA prevB list)
   (setf nextElem (CAR list)) 
   (setf newA (CAR nextElem))
   (setf newB (CADR nextElem)) 
   (if (and (equal (- prevA 1) newA) 
            (equal (+ prevB 1) newB))
               (incf *win* 1))
   (cond ((equal *win* 3) t)
         ((null list) nil)
         (t (and (setf prevA newA)
                 (setf prevB newB)
                 (winDiagonalDown prevA prevB (rest list))))))



;;;Passes first arguments to winDiagonalUp.
(defun winDiagonal_Up (list)
    (setf firstElem (CAR list))
    (setf firstA (CAR firstElem))
    (setf firstB (CADR firstElem))
    (cond ((winDiagonalUp firstA firstB (rest list)) 
           (printWinner) T)
          (t (win_Horizontal list))))     

;;;Check for Upward sloping win.	
(defun winDiagonalUp (prevA prevB list)
   (setf nextElem (CAR list))
   (setf newA (CAR nextElem))
   (setf newB (CADR nextElem))
   (if (and (equal (+ prevA 1) newA)
            (equal (+ prevB 1) newB))
            (incf *win* 1))
   (cond ((equal *win* 3) t)
         ((null list) nil)       
         (t (and (setf prevA newA)
                 (setf prevB newB)
                 (winDiagonalUp prevA prevB (rest list))))))


;;;Passes first arguments to winHorizontal.
(defun win_Horizontal (list)
    (setf firstElem (CAR list))
    (setf firstA (CAR firstElem))
    (setf firstB (CADR firstElem))
  (cond ((winHorizontal firstA firstB (rest list))
         (printWinner) T)
        (t (win_Vertical list))))

;;;Check for a Horizontal win.
(defun winHorizontal (prevA prevB list)
   (setf nextElem (CAR list))
   (setf newA (CAR nextElem))
   (setf newB (CADR nextElem))
   (if (and (equal (+ prevA 1) newA)
            (equal prevB newB))
            (incf *win* 1))
   (cond ((equal *win* 3) t)
         ((null list) nil)
         (t (and (setf prevA newA)
                 (setf prevB newB)
                 (winHorizontal prevA prevB (rest list))))))


;;;Passes first arguments to winVertical.
(defun win_Vertical (list)
    (setf firstElem (CAR list))
    (setf firstA (CAR firstElem))
    (setf firstB (CADR firstElem))
  (cond ((winVertical firstA firstB (rest list))
         (printWinner) T)
        (t (and (format t "~%~%There is no winner yet. Please continue the game.") nil))))

;;;Check for a Vertical win.
(defun winVertical (prevA prevB list)
   (setf nextElem (CAR list))
   (setf newA (CAR nextElem))
   (setf newB (CADR nextElem))
   (if (and (equal prevA newA)
            (equal (+ prevB 1) newB))
               (incf *win* 1))            
   (cond ((equal *win* 3) t)
         ((null list) nil)
         (t (and (setf prevA newA)
                 (setf prevB newB)  
                 (winVertical prevA prevB (rest list))))))

(defun printWinner() 
  (if (eq *oponent* 'PLAYER)
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
   ##     #######   #######     ########  #######   #######   ######  ######## ####~%~%")))
                                     	
        
;;;--------ENDWINCHECKING---------------------------------------------
