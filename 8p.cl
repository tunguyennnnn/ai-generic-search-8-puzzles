(require :AI)
(compile-file :AI)
(compile-file :utilities)

;; usage:
;; to search using depth first search: call (ai-search <State> 'dfs #'8p-funcs)
;; to search using breadth first search: call (ai-search <State> 'brfs #'8p-funcs)
;; to search using best first search: call (ai-search <State> 'bfs #'8p-funcs)
;; to search using A*: call (ai-search <State> 'h #'8p-funcs) or (ai-search <State> 'h #'8p-funcs <index of heuristic function>) 
;; <state> example: #(1 2 3 8 5 6 7 X 4) and <index of heuristic function>: 0 -> misplaced tiles
;																			1 -> manhattan-distance
;																			2 -> minimum cost of manhattan-distance and misplaced tiles
; 																			3 -> inadmissible heristic
;																			4 -> linear conflict heuristic


(setf *goal-puzzle* #(1 2 3 8 X 4 7 6 5))  ;; goal of the 8 puzzle problem
										   ;; represent 1 2 3
											;     		8 X 4
											;			7 6 5

(defun 8p-actions (puzzle) ;; return next position of X
	(let ((blank-pos (position 'X puzzle)))
		(case blank-pos
			(0 '(1 3)) ;;  if X is at 0 -> X can move right (to 0) or down (to 3)
			(1 '(0 2 4))
			(2 '(1 5))
			(3 '(0 4 6))
			(4 '(1 3 5 7))
			(5 '(2 4 8))
			(5 '(2 4 8))
			(6 '(3 7))
			(7 '(4 6 8))
			(8 '(5 7)))))
			
(defun 8p-successor (puzzle pos) ;; give a current state and an action -> generate next state and cost leading to new state
	(let ((blank-pos (position 'X puzzle))
		  (re-seq (copy-seq puzzle)))
		(array-swap re-seq pos blank-pos)
		(values re-seq 1)))
		

(defun 8p-goal-test (puzzle)
	(equalp puzzle *goal-puzzle*))
			
(defun 8p-funcs (search-type num) ;; return list of functions to be used in general search function
	(values #'8p-goal-test
			#'8p-actions
			#'8p-successor
			(if (member search-type '(dfs brfs bfs)) ;;if not heuristic search
				#'(lambda (state) 0) ;-> heuristic cost is always 0
				(case num
					(0 #'8p-miss-placed)
					(1 #'8p-manhattan-distance)
					(2 #'8p-manhattan-misplaced)
					(3 #'8p-non-admissible)
					(4 #'8p-linear-conflict)))
			#'equalp))



(defun 8p-goal-test (puzzle)
	(equalp puzzle *goal-puzzle*))


;;;;;; HEURISTIC FUNCTIONS
(defun 8p-miss-placed (puzzle)
	(do ((i 0 (+ i 1))
		 (counter 0)
		 (end (length puzzle)))
	    ((eql i end) counter)
	    (when (not (eql (aref puzzle i) (aref *goal-puzzle* i)))
	    	(incf counter))))

(defun 8p-manhattan-distance (puzzle)
	(reduce #'+ (map 'vector
				#'(lambda (x)
	 				(let ((pos (position x puzzle))
	 					  (ref-pos (position x *goal-puzzle*)))
						(if (or (eql pos ref-pos) (eql x 'X))
						 	0
						 	(multiple-value-bind (y-pos x-pos) (floor (abs (- pos ref-pos)) 3)
						 		(+ x-pos y-pos))))) 
				puzzle)))

(defun 8p-manhattan-misplaced (puzzle)
	(min (8p-manhattan-distance puzzle)
		 (8p-miss-placed puzzle)))

		 
;;; non admissible  heuristic 
(defun 8p-non-admissible (puzzle)
	(reduce #'+ (map 'vector
				#'(lambda (x)
	 				(let ((pos (position x puzzle))
	 					  (ref-pos (position x *goal-puzzle*)))				
						 	(multiple-value-bind (y-pos x-pos) (floor (abs (- pos ref-pos)) 3)
						 		(+ x-pos y-pos))))
				puzzle)))

	
;; linear conflict heuristic -> improvement of manhattan-distance heristic	
(setf *conflict-joint* (make-hash-table))
(setf (gethash 1 *conflict-joint*) '((0 7 8) . (0 2 3)) ;; possible conflict pairs in row and colums ex of the first hash tag:
	  (gethash 2 *conflict-joint*) '((1 6) . (0 3))  	;;										if 1 is at column 0 then if 7 or 8 is at column 0 			
	  (gethash 3 *conflict-joint*) '((2 4 5) . (0))		;;												-> can have conflict -> (0 7 8) where 0 is number of column, 7, 8 are tiles that can cause conflict
	  (gethash 4 *conflict-joint*) '((2 5 6) . (1 8))	;;										if 1 is at row 0 then if 2  or 3 is at row 0 			
	  (gethash 3 *conflict-joint*) '((2 4 5) . (0))		;;												-> can have conflict -> (0 2 3) where 0 is number of column, 2, 3 are tiles that can cause conflict
	  (gethash 5 *conflict-joint*) '((2) . (2 6 7))
	  (gethash 6 *conflict-joint*) '((1) . (2 7))		
	  (gethash 7 *conflict-joint*) '((0 8) . (2)) 
	  (gethash 8 *conflict-joint*) '((0) . (1))
	  )
	  
	  
(defun get-conflict (el)
	(let ((conflict (gethash el *conflict-joint*)))
		(values (car conflict) (cdr conflict))))


;; for each titles from 1 to 8: 
;; if the tile is in right position -> skip
;; find number of steps can bring it back
;; and if it and its conflict tiles are in the goal rows or columns 
;; if they do -> check if swapping is needed 
;; if swapping is needed -> add 2 to the its manhantan distance cost
(defun 8p-linear-conflict (puzzle)
	(apply '+ 
		(mapcar  
			#'(lambda (el)
				(let ((pos (position el puzzle))
					(ref-pos (position el *goal-puzzle*)))
						(if (or (eql pos ref-pos) (eql el 'X))
							0
							(multiple-value-bind (y x) (floor (abs (- pos ref-pos)) 3)
								(multiple-value-bind (x-conflict y-conflict) (get-conflict el)
									(let ((cost (+ x y)))
										(multiple-value-bind (y-coor x-coor) (floor pos 3)
											(cond ((eql x-coor (car x-conflict)) 
												   (+ cost (apply '+ (mapcar #'(lambda (peer)
																				 (let ((pos-peer (position peer puzzle)))
																						(multiple-value-bind (y-peer x-peer) (floor pos-peer 3)
																							(if (eql x-peer (car x-conflict))
																								(cond ((and (> y-coor y-peer) 
																											(> 7  el) 
																											(not (eql pos-peer (position peer *goal-puzzle*)))) 2)
																									  ((and (< y-coor y-peer) 
																											(<= 7 el) 
																											(not (eql pos-peer (position peer *goal-puzzle*)))) 2)
																									  (t 0))
																								0)))) (cdr x-conflict)))))
												  ((eql y-coor (car y-conflict))
												  		(+ cost (apply '+ (mapcar #'(lambda (peer)
												  									(let ((pos-peer (position peer puzzle)))
												  										(multiple-value-bind (y-peer x-peer) (floor pos-peer 3)
												  											(if (equal y-peer (car y-conflict))
												  												(cond ((and (> x-coor x-peer) 
												  															(>= 3 el) 
												  															(not (eql pos-peer (position peer *goal-puzzle*)))) 2)
												  													  ((and (< x-coor x-peer) 
												  													  		(< 3 el) 
												  													  		(not (eql pos-peer (position peer *goal-puzzle*)))) 2)
												  													  (t 0))
												  												0)))) (cdr y-conflict)))))
												  (t cost)))))))))
					#[0 8])))

					
;; funcs to generate a random state					
(defun generate-board (steps)
	(let ((puzzle (copy-seq *goal-puzzle*)))
		(do ((i 0 (1+ i)))
			((equalp i steps) puzzle)
			(let ((action-list (8p-actions puzzle)))
				(let ((move (random-elt action-list)))
					(setf puzzle (8p-successor puzzle move)))))))

(setf x (generate-board 50))
					
(print (ai-search x 'hs #'8p-funcs 0))
(print (ai-search x 'hs #'8p-funcs 1))
(print (ai-search x 'hs #'8p-funcs 2))
(print (ai-search x 'hs #'8p-funcs 3))
(print (ai-search x 'hs #'8p-funcs 4))