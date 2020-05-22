;;;; @file connect4.lisp
;;; Variable difficulty Connect 4
;;; A verison of the popular board game which involves an AI player
;;; vs. one human player.



(defun load-relative(filename)
	"Loads files using relative file paths."
  
  ; Turn off load and compile messages.
	(setf *load-verbose* nil)
  (setf *load-print* nil)
  (setf *compile-verbose* nil)
  (setf *compile-print* nil)

  ; Compiles and loads relative file paths.
  (load (compile-file (merge-pathnames filename *load-truename*))))

;; Ensure necessary global variables are set to supress potential compiler warnings.
(defun reset-game()
  (setf game '())
  (setf first 0)
  (setf difficulty 0)
  (setf move-positions '())
  (setf win nil)
  (setf gameover nil)
)

(defvar move-positions)
(defvar difficulty)

(defvar game '())
(defvar first '())
(defvar firstElem)
(defvar *compMoves*)
(defvar *playerMoves*)
(defvar *oponent*)
(defvar *win*)
(defvar testList)
(defvar winDiagonalDown)
(defvar winDiagonal_up)
(defvar winHorizontal)
(defvar winVertical)
(defvar win)
(defvar firstA)
(defvar firstB)
(defvar nextElem)
(defvar newA)
(defvar newB)
(defvar input)
(defvar play)
(defvar ai-last-move)
(defvar gameover)

; Call reset-game to ensure clean vars.
(reset-game)


;; ------------------------------ LOAD THE NECESSARY FILES ------------------------------
(let ((*error-output* (make-string-output-stream)))
  ;; Compile & load helper functions.
  (load-relative "inc/helper.lisp")

  ;; Compile & load AI engine file.
  (load-relative "inc/ai.lisp")

  ;; Compile & load output file.
  (load-relative "inc/output.lisp")

  ;; Compile & load file that checks gameplay for wins.
  (load-relative "inc/winCheck2.lisp")

  ;; Compile & load helper main program control file.
  ;; Load main.lisp last, so it has all its dependencies.
  (load-relative "inc/main.lisp")

  ;; Print compile errors to file, rather than to the std-out.
  (with-open-file (str (merge-pathnames "tmp/compile.log" *load-truename*)
                 :direction :output
                 :if-exists :append
                 :if-does-not-exist :create)
    (format str (concatenate 'string "~%Compile Time " (write-to-string (get-universal-time))
      " Results:~%------------------------------------------------------~%"
      (get-output-stream-string *error-output*)) "~%~%")))

;; --------------------------- CALL MAIN AND EXECUTE PROGRAM ----------------------------

;;; If main is specified outside of a function nothing is required here. If specified
;;; inside a function then call the function. Ex: (main).