(require :utilities)
;;; SearchNode class -> storing state, parent, action to the state and the cost to the state.
(defclass SearchNode ()
	((state :accessor sn-state 
			:initarg :state
			:initform nil)

	 (action :accessor sn-action
	 		:initarg :action
	 		:initform nil)

	 (parent :accessor sn-parent
	 		:initarg :parent
	 		:initform nil)
	 (action-cost :accessor action-cost
	 			  :initarg :action-cost
	 			  :initform 0)))

;; constructor: add cost to the action-cost slot 
(defmethod initialize-instance :after ((sn SearchNode) &key)
	(setf (action-cost sn) 
			(aif (sn-parent sn)
				(+ (action-cost it) (action-cost sn))
				(action-cost sn))))

;; return full path of a search node (actions lead to it, states and costs)
(defmethod path ((x  SearchNode))
	(let ((current-state (list (cons (sn-action x) (sn-state x)) (action-cost x))))
		(if (null (sn-parent x))
			current-state
			(cons (path (sn-parent x)) current-state))))
		  

		  
		  
		  
		  
		  
		  
		  
;;; piority queue structure -> used to represent stack, queue and piority queue -> determined by the compare-func -> stored in global *search-algorithms*
;;; for stack compare-func always return true to push new element to front 
;;; for queue compare-func always return nil to push new element to end 
;;; for piority queue, compare-func return true if true if new element's cost is smaller than current head of data

(setf *search-algorithms* (make-hash-table))

;; depth first search always add new node to head of the queue -> thus compare-func always returns true
(setf (gethash 'dfs *search-algorithms*) #'(lambda (x y) t))

;; breath first search always push new node to the tails position -> thus compare-func always return nil (false)
(setf (gethash 'brfs *search-algorithms*) #'(lambda (x y) nil))

;; unless the type search is dfs or brfs -> the compare-func will be return true if 
;; new state has lower cost than an element in the queue
(defun make-piority-func (type)
	(aif (gethash type *search-algorithms*)
		it 
		#'(lambda (x y) (< x y))))
		
;; structure to represent queue and store compare and equal functions
;; equal function is dependent on the type of problems :for example 8 puzzle board is represented by 
;; array -> #'equalp. however if the problem is represented by non premititve objects -> need a equal-funcs 
;; the equal-func is used to check if a state has been in the queue to take further actions	
(defstruct piority-queue 
	(:data (list nil nil)) 
	(:compare-func #'(lambda (new-cost cost) t))	
	(:equal-func #'equalp))

	
;;input node -> state of problem and cost of the state
;; base case: if queue is empty -> push state and cost to it 
(defun pq-push (pq node &optional (cost 0))
	(let ((q (piority-queue-data pq))
		  (compare-func (piority-queue-compare-func pq)) 
		  (equal-func (piority-queue-equal-func pq)))
		(if (null (car q))
			(setf (car q) (cons (list node cost) (car q)))
			(do ((lst (car q) (cdr lst)) (accum nil))
				((null lst) (setf (car q) (reverse (cons (list node cost) accum))))
				(cond ((funcall equal-func node (caar lst)) ;;if repeated states 
						(when (not (funcall compare-func cost (cadr (car lst)))) 
						;;if new cost is greater than old cost -> do nothing and return
							(return q)))
					  ((funcall compare-func cost (cadr (car lst))) ;;if new cost < current cost
																	;; place the state at that slot and remove repeated state
																	;; from the rest of the queue if any
						(progn
							(setf (car q) (nconc (reverse (cons (list node cost) accum)) 
												 (remove-if #'(lambda (node) (funcall equal-func node (car node))) lst)	
													))
							(return q)))
					  (t 
							(setf accum (cons (car lst) accum))))))))
							

;; pop head of queue							
(defun pq-pop (pq)
	(let ((q (piority-queue-data pq)))
		(let ((top (car (car q))))
			(setf (car q) (cdr (car q)))
			(car top))))

(defun pq-length (pq)
	(length (car (piority-queue-data pq))))

(defun pq-empty? (pq)
	(null (car (piority-queue-data pq))))
	

		
;;; closed list structure -> data: list of states visited
;;;						  -> equal-func: a predicate to verify if a state is in data list
(defstruct closed-list (:data nil) (:equal-func #'equalp))

(defun in-closed-list? (cl state)
	(let ((data (closed-list-data cl)))
		(dolist (element data)
			(when (funcall (closed-list-equal-func cl) state element)
					(return t)))))

(defun add-to-closed-list (cl state)
	(setf (closed-list-data cl) (cons state (closed-list-data cl))))
	
(defun closed-list-length (cl)
	(length (closed-list-data cl)))
				


;;; generic search functions 
;;; input: intial state, search type 'dfs -> depth first search 
;;;									 'brfs -> breath first search 
;;;									 'bfs -> best first search
;;;									 otherwise -> search with heuristic -> which allows to pass an optional argument
;;;												  							for heuristic type
;;; and a function which takes search type and return multiple functions the search problem
;;; output: path to the goal state -> including action leads to the state and cost


;;; functions expected from the problems:
;;; goal-test -> return true if a state of problem is the goal
;;; actions -> return list of possible actions can be taken to next states ex: in 8 puzzle, if space is at position 0 -> actions returns '(1 3) which represent '(right down)
;;; successor -> return states based on the actions taken from previous state 
;;; heuristic -> heuristic cost of a state -> for dfs, brfs, and bfs -> heuristic function should return 0
;;; equal? -> functions used to compared if 2 states are equal -> it is in the form of a function because 
;;; state of a problems can be represented differently 
(defun ai-search (initial-state search-type problem-funcs &optional (heuristic-num 0))
	(multiple-value-bind (goal-test actions successor heuristic equal?) (funcall problem-funcs search-type heuristic-num) ;;-> list of functions of the problem
		(let ((start-node (make-instance 'SearchNode :state initial-state)))
			(if (funcall goal-test initial-state) ;; if intial state is goal -> return goal
				start-node
				;; otherwise makes open list with initial state and close list
				(let ((opened-list (make-piority-queue :compare-func (make-piority-func search-type) :equal-func equal?))
					  (closed-list (make-closed-list :equal-func equal?)))
					(pq-push opened-list start-node 0)
					(while (not (pq-empty? opened-list))
						(let* ((parent (pq-pop opened-list))
							   (parent-state (sn-state parent)))
							(when (not (in-closed-list? closed-list parent-state))
								(add-to-closed-list closed-list parent-state)
								;; for each next state of current state (parent) -> check if it's goal state, otherwise check if it's in close list, if its not -> push it to open list
								(do ((action-list (funcall actions parent-state) (cdr action-list)))
									((null action-list) nil)
									(multiple-value-bind (new-s cost) (funcall successor parent-state (car action-list))
										(when (not (in-closed-list? closed-list new-s))
											(let ((new-n (make-instance 'SearchNode :state new-s :parent parent :action (car action-list) :action-cost cost))) 
												(if (funcall goal-test new-s)
													(progn
														(print (closed-list-length closed-list)) ;; -> number of states visited
														(return-from ai-search (path new-n)))
													(pq-push opened-list new-n (+ cost (if (member search-type '(brfs dfs bfs)) 0 (action-cost parent)) (funcall heuristic new-s))))))))))))))))
		







