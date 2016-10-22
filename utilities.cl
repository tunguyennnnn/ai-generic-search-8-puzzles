(defun mapa-b (fn a b &optional (step 1)) ;; (mapa-b 0 10 2) -> '(0 2 4 8 10)
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map0-n (fn n)
  (mapa-b fn 0 n))
  
  
(defmacro while (test &body body)
	`(do ()
		((not ,test))
		,@body))
		
		
(set-macro-character #\] (get-macro-character #\)))

(set-dispatch-macro-character #\# #\[ ;; #[0 10] -> #(0 1 2 3 4 5 6 7 8 9 10)
	#'(lambda (stream char1 char2)
		(let ((accum nil)
			  (pair (read-delimited-list #\] stream t)))
			(do ((i (ceiling (car pair)) (1+ i)))
				((> i (floor (cadr pair)))
				 (list 'quote (nreverse accum)))
				(push i accum)))))
				
				
(defun array-swap (array i1 i2)
  (let ((tmp (aref array i1)))
    (setf (aref array i1) (aref array i2)
	  (aref array i2) tmp)))

;; (aif (+ 1 3) (print it)) ~= (print 4) -> it refers to result of (+ 1 3)
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))
	 
(defun random-elt (choices)
  (elt choices (random (length choices))))