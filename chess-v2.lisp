(defparameter *size* 8)

(defstruct chess-piece
  (name nil :type symbol)
  (pos '() :type list)
  (color nil :type boolean)
  (threats '() :type list)
  )

(defun xor (a b) (not (eql (not a) (not b))))

(defun no-op (a b) a)

(defun bounds (x y)
  (cond
    ((or (< x 0) (> (+ x 1) *size*)) nil)
    ((or (< y 0) (> (+ y 1) *size*)) nil)
    (t t)))


(defun line (srow scol mat i j color) 
    (let* ((next-i (funcall srow i 1)) (next-j (funcall scol j 1)) (in-bounds (bounds next-i next-j))) 
      (cond
	((and in-bounds (not (null (aref mat next-j next-i)))) (list next-i next-j)) ;should return target piece found 
        (in-bounds (line srow scol mat next-i next-j color)) ; continue searching
        (t (list i j)) ; out of bounds return last valid position
        )))

(defun make-king (mat i j color)
  (let ((piece (make-chess-piece :name 'king :pos (list i j) :color color))) ; king threats no one
    (setf (aref mat j i) piece) ; setf substitutes the provisory quoted funcion symbol to the final complete struct and they share the same memory
    piece))
					
;rook + bishop
(defun make-queen (mat i j color)
  (let ((piece (make-chess-piece :name 'queen :pos (list i j) :color color :threats
   (list 
    ;diagonals
    (line '+ '+ mat i j color)
    (line '- '- mat i j color)
    (line '+ '- mat i j color)
    (line '- '+ mat i j color)

    ;lines
     (line '+ 'no-op mat i j color)
     (line '- 'no-op mat i j color)
     (line 'no-op '+ mat i j color)
     (line 'no-op '- mat i j color)))))
    (setf (aref mat j i) piece)
    piece))
    
(defun make-bishop (mat i j color)
  (let ((piece (make-chess-piece :name 'bishop :pos (list i j) :color color :threats
    (list
     (line '+ '+ mat i j color)
     (line '- '- mat i j color)
     
     (line '+ '- mat i j color)
     (line '- '+ mat i j color)))))
    
    (setf (aref mat j i) piece)
    piece))

							   


(defun make-knight (mat i j color)
  (let ((res '()))
    ; upper
    (if (bounds (+ i 2) (- j 1)) (push (list (+ i 2) (- j 1)) res))
    (if (bounds (+ i 2) (+ j 1)) (push (list (+ i 2) (+ j 1)) res))

    ;lower
    (if (bounds (- i 2) (- j 1)) (push (list (- i 2) (- j 1)) res))
    (if (bounds (- i 2) (+ j 1)) (push (list (- i 2) (+ j 1)) res))

    ;left
    (if (bounds (- i 1) (- j 2)) (push (list (- i 1) (- j 2)) res))
    (if (bounds (- i 1) (+ j 2)) (push (list (- i 1) (+ j 2)) res))

    ;right
    (if (bounds (+ i 1) (- j 2)) (push (list (+ i 1) (- j 2)) res))
    (if (bounds (+ i 1) (+ j 2)) (push (list (+ i 1) (+ j 2)) res))

    (let ((piece (make-chess-piece :name 'knight :pos (list i j) :color color :threats res)))
      (setf (aref mat j i) piece)
      piece)))


(defun make-rook (mat i j color)
  ;(format t "Im a rook ~A ~A" i j)
  (let ((piece   (make-chess-piece :name 'rook :pos (list i j) :color color :threats
     (list
      (line '+ 'no-op mat i j color)
      (line '- 'no-op mat i j color)
      (line 'no-op '+ mat i j color)
      (line 'no-op '- mat i j color)))))
    (setf (aref mat j i) piece)
    piece))

;(format nil "pawn ~A~A ~A" i j color)
(defun make-pawn (mat i j color)
  (format t "~A i:~A j:~A" i j color)
  (let ((res '()))     
    ; Color t
    (if (and (bounds (+ i 1) (+ j 1)) color) (push (list (+ i 1) (+ j 1)) res)) 
    (if (and (bounds (- i 1) (+ j 1)) color) (push (list (- i 1) (+ j 1)) res))

    (if (and (bounds (+ i 1) (- j 1)) (not color)) (push (list (+ i 1) (- j 1)) res)) 
    (if (and (bounds (- i 1) (- j 1)) (not color)) (push (list (- i 1) (- j 1)) res)) 

    (let ((piece (make-chess-piece :name 'pawn :pos (list i j) :color color :threats res)))
      (setf (aref mat j i) piece)
	    piece)))



(defun mount-chess (lst &optional (mat (make-array (* *size* *size*) :initial-element nil)) (i 0) cache)
  (cond
    ((or (equal i (length mat)) (null lst)) (list (make-array (list *size* *size*) :displaced-to mat) cache))
    ((typep (car lst) 'string)
     (let ((head (car lst)))
       (cond
	 ((eql (length head) 0) (mount-chess (cdr lst) mat i cache))
	 (t
	  (let ((piece (char head 0)))
	    ;(print (char-downcase piece))
	    (setf (aref mat i)
		  (case (char-upcase piece)
		    (#\K 'make-king)
		    (#\Q 'make-queen)
		    (#\B 'make-bishop)
		    (#\N 'make-knight)
		    (#\R 'make-rook)
		    (#\P 'make-pawn)
		    (otherwise (error "Piece ~a was not recognize" piece))
	    ))
	  (mount-chess (cons (subseq head 1) (cdr lst)) mat (+ i 1) (cons (list (mod i 8) (truncate i 8) (lower-case-p piece) piece) cache)))));; be aware list is inverted
     ))
    ((typep (car lst) 'integer) (mount-chess (cdr lst) mat (+ i (car lst)) cache))
    ))

(defun range (lst)
  (cond
    ((null lst) nil)
    ((null (car (cdr lst))) nil)
    (t
     (let* ((board (car lst)) (pos (car (cdr lst)))) ;; Y is first (row major)
       (map 'list
	    (lambda (target) (let ((y (car (cdr target))) (x (car target)) (color (car (cdr (cdr target)))))  (print target) (funcall (aref board y x) board x y color))) pos)))))


(defun eval-threat (piece board &optional (threats (chess-piece-threats piece)))
  (cond
    ((null piece))
    ((null board))
    ((null threats))
    (t
     (let* ((head (car threats)) (i (car head)) (j (car (cdr head))) (target (aref board j i)))
       (if target
	   (if (xor (chess-piece-color piece) (chess-piece-color target))
	       (progn
		 (if (eql (chess-piece-name target) 'king) (format t "***~%"))
		 (format t "~A~A~A -> ~A~A~A ~%"
		       (if (chess-piece-color piece) "black " "white ")
		       (chess-piece-name piece)
		       (chess-piece-pos piece)
		       (if (chess-piece-color target) "black " "white ")
		       (chess-piece-name target)
		       (chess-piece-pos target))
		 (if (eql (chess-piece-name target) 'king) (format t "***~%"))))))
     (eval-threat piece board (cdr threats)))))
    

(defun print-threats* (board chess-lst)
    (cond
      ((null chess-lst))
      (t
       (eval-threat (car chess-lst) board)
       (print-threats* board (cdr chess-lst)))))

(defun print-threats (chess-notation)
  (let* ((board (mount-chess chess-notation)) (chess-lst (range board)))
    ;; ATENTION - mount chess return a (list board auxlist) so it must be unpacked 
    (print-threats* (car board) chess-lst)))



(let* (
       (t0 '("rnbqkbnr" "pppppppp" 8 8 8 8 "PPPPPPPP" "RNBQKBNR"))
       (t1 '("rnbqkbnr" 8 4 "r" 3 8 8 8 8 "RNBQKBNR"))
       (t2 '("rnbkqbnr" 8 8 8 8 8 8 "RNBQKBNR"))
       
       (board (mount-chess t1))
       (vector (range board)))
  (print board)
  (print vector)
  (print-threats t1))


; run with sbcl --load to REPL or --script to run as script


;(print (chess '("tcbdrbct" "pppppppp" 8 8 8 8 "PPPPPPPP" "TCBDRBCT")))
;(print (mount-chess '("rkbqkbkr" "pppppppp" 8 8 8 8 "PPPPPPPP" "RKBQKBKR")))

; ((board (mount-chess '("rnbqkbnr" "pppppppp" 8 8 8 8 "PPPPPPPP" "RNBQKBNR")))
;(print-threats '("rnbkqbnr" 8 8 8 8 8 8 "RNBQKBNR"))
