(require :AI)
(compile-file :AI)
(compile-file :utilities)
(defvar *city-map* (make-hash-table))

(defun defcity (city-name &rest near-by-cities)
  (setf (gethash city-name *city-map*)
         near-by-cities))

(defcity 'Arad '(Zerind . 75) '(Timisoara . 118) '(Sibiu . 140))
(defcity 'Zerind '(Oradea . 71) '(Arad . 75))
(defcity 'Timisoara '(Lugoj . 111) '(Arad . 118))
(defcity 'Lugoj '(Mehadia . 70) '(Timisoara . 111))
(defcity 'Mehadia '(Dobreta . 75) '(Lugoj . 70))
(defcity 'Oradea '(Sibiu . 151) '(Zerind . 71))
(defcity 'Dobreta '(Craiova . 120) '(Mehadia . 75))
(defcity 'Craiova '(Rimnicu-Vilcea . 146) '(Pitesti . 138) '(Dobreta . 120))
(defcity 'Sibiu '(Rimnicu-Vilcea . 80) '(Fagaras . 99) '(Oradea . 151) '(Arad . 140))
(defcity 'Rimnicu-Vilcea '(Sibiu . 80) '(Craiova . 146) '(Pitesti .  97))
(defcity 'Pitesti '(Bucharest . 101) '(Craiova . 138) '(Rimnicu-Vilcea . 97))
(defcity 'Fagaras '(Bucharest . 211) '(Sibiu . 211))
(defcity 'Bucharest '(Glurgiu . 90) '(Urziceni . 85) '(Pitesti . 101) '(Fagaras . 211))

(defun make-queue () (cons nil nil))



(defun city-actions (state)
	(nreverse (map0-n #'identity (1- (length (gethash state *city-map*))))))
	
(defun city-goal-test (state)
	(eql state 'Bucharest))


(defun city-successor (search-type type)
	(if (member search-type '(dfs brfs))
		#'(lambda (s a)
				(let ((child (nth a (gethash s *city-map*))))
					(values (car child) 0)))
		#'(lambda (s a)
				(let ((child (nth a (gethash s *city-map*))))
					(values (car child) (cdr child))))))
		

(defun city-heuristic (state)
	(case state
		('Arad 336)
		('Bucharest 0)
		('Zerind 374)
		('Timisoara 329)
		('Lugoj 224)
		('Mehadia 241)
		('Oradea 380)
		('Dobreta 242)
		('Craiova 160)
		('Sibiu 253)
		('Rimnicu-Vilcea 193)
		('Pitesti 98)
		('Fagaras 178)
		(t 100)))
		
		
(defun city-funcs (search-type)
	(values #'city-goal-test
			#'city-actions
			(city-successor search-type)
			(if (member search-type  '(dfs brfs bfs))
				#'(lambda (x) 0)
				#'city-heuristic)
			#'eql
			))	
	