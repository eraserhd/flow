;; Digits
(define (digit? c)
  (and (char>=? c #\0)
       (char<=? c #\9)))

(define (digit->integer c)
  (- (char->integer c)
     (char->integer #\0)))

;; Grids
(define (make-grid width . initial-values)
  (list->vector (cons width initial-values)))

(define grid-copy vector-copy)

(define (grid-width grid)
  (vector-ref grid 0))

(define (grid-height grid)
  (quotient
    (- (vector-length grid) 1)
    (grid-width grid)))

(define make-index cons)
(define index-row car)
(define index-column cdr)

(define (index? index)
  (and (pair? index)
       (fixnum? (car index))
       (fixnum? (cdr index))))

(define (grid-cell-index/internal grid index)
  (+ (* (index-row index)
        (grid-width grid))
     (index-column index)
     1))

(define (grid-ref grid index)
  (vector-ref grid (grid-cell-index/internal grid index)))

(define (grid-set! grid index value)
  (vector-set! grid (grid-cell-index/internal grid index) value))

(define (parse-grid . rows)
  (let* ((width (string-length (car rows)))
	 (colors-found '())
	 (starts '())
	 (char->cell-value (lambda (c)
			     (cond
			       ((or (char=? #\space c)
				    (char=? #\* c))
				'empty)
			       ((digit? c)
				(cond
				  ((memq c colors-found)
				   `(goal . ,(digit->integer c)))
				  (else
				   (set! colors-found (cons c colors-found))
				   `(start . ,(digit->integer c)))))
			       (else
				 (raise (string-append
					  "Invalid character '"
					  (make-string 1 c)
					  "' found in state specification."))))))
	 (cell-values (map char->cell-value (string->list (apply string-append rows)))))
    (apply make-grid width cell-values)))

(define (grid-indices grid)
  (let loop ((i 0)
	     (j 0)
	     (indices '()))
    (cond
      ((= i (grid-height grid))
       (reverse indices))
      ((= j (grid-width grid))
       (loop (+ i 1) 0 indices))
      (else
       (loop i (+ j 1) (cons (make-index i j) indices))))))

(define (grid-detect grid proc)
  (call/cc
    (lambda (return)
      (for-each
	(lambda (index)
	  (if (proc (grid-ref grid index))
	    (return index)))
	(grid-indices grid))
      #f)))

(define (start-cell? cell)
  (and (pair? cell)
       (eq? 'start (car cell))))

(define (grid-cell-color grid index)
  (let loop ((index index)
	     (cell-value (grid-ref grid index)))
    (cond
      ((and (pair? cell-value)
	    (memq (car cell-value) '(start goal)))
       (cdr cell-value))
      ((index? cell-value)
       (loop cell-value (grid-ref grid cell-value)))
      (else
       #f))))

(define (grid-cell-empty? grid index)
  (eq? 'empty (grid-ref grid index)))

(define (grid-index-valid? grid index)
  (and (>= (index-row index) 0)
       (>= (index-column index) 0)
       (< (index-row index) (grid-height grid))
       (< (index-column index) (grid-width grid))))

(define (grid-possible-moves grid index)
  (let loop ((deltas '((-1 . 0) (+1 . 0) (0 . -1) (0 . +1)))
	     (possible-moves '()))
    (cond
      ((null? deltas)
       possible-moves)
      ((not (index? index))
       '())
      (else
       (let* ((possible-move (make-index
			       (+ (index-row index)
			          (index-row (car deltas)))
			       (+ (index-column index)
				  (index-column (car deltas))))))
	 (cond
	   ;; Can't move off the board
	   ((not (grid-index-valid? grid possible-move))
	    (loop (cdr deltas) possible-moves))

	   ;; Can move to empty cell
	   ((grid-cell-empty? grid possible-move)
	    (loop (cdr deltas) (cons possible-move possible-moves)))

	   ;; Can move to goal if goal is same color
	   ((and (pair? (grid-ref grid possible-move))
		 (eq? 'goal (car (grid-ref grid possible-move))))
	    (let ((goal-color (cdr (grid-ref grid possible-move)))
		  (start-color (grid-cell-color grid index)))
	      (if (equal? goal-color start-color)
		(loop (cdr deltas) (cons possible-move possible-moves))
		(loop (cdr deltas) possible-moves))))

	   ;; Otherwise, can't move there
	   (else
	    (loop (cdr deltas) possible-moves))))))))

(define (grid-solved? grid)
  (not (grid-detect grid
	 (lambda (cell-value)
	   (or (eq? 'empty cell-value)
	       (and (pair? cell-value)
		    (eq? 'goal (car cell-value))))))))

(define (grid-solve grid)

  (define (solve/internal grid index)
    (if (grid-solved? grid)
      grid
      (let loop ((possible-moves (grid-possible-moves grid index)))
	(cond
	  ((null? possible-moves)
	   #f)
	  (else
	   (let* ((this-move (car possible-moves))
		  (new-grid (let ((g (grid-copy grid)))
		              (grid-set! g this-move index)
			      g))
		  (original-cell-value (grid-ref grid this-move))
		  (original-goal? (and (pair? original-cell-value)
				       (eq? 'goal (car original-cell-value))))
		  (next-index (if (not original-goal?)
				this-move
				(let* ((next-goal (grid-detect new-grid
						    (lambda (cell-value)
						      (and (pair? cell-value)
							   (eq? 'goal (car cell-value))))))
				       (goal-color (if next-goal
						     (cdr (grid-ref new-grid next-goal))
						     #f))
				       (next-start (if next-goal
						     (grid-detect new-grid
						       (lambda (cell-value)
							 (and (pair? cell-value)
							      (eq? 'start (car cell-value))
							      (= goal-color (cdr cell-value)))))
						     #f)))
				  next-start)))
		  (sub-result (solve/internal new-grid next-index)))
	     (if sub-result
	       sub-result
	       (loop (cdr possible-moves)))))))))

  (solve/internal grid (grid-detect grid start-cell?)))

(define (grid->string grid)
  (let* ((s-width (+ 2 (* 2 (grid-width grid))))
	 (s-height (+ 1 (* 2 (grid-height grid))))
	 (s (make-string (* s-width s-height) #\-)))

    (let loop ((i 0))
      (if (= i s-height)
	#f
	(begin
	  (string-set! s (+ (* i s-width) (- s-width 1)) #\newline)
	  (loop (+ i 1)))))

    (for-each
      (lambda (index)
	(let* ((i (+ 1 (* 2 (index-row index))))
	       (j (+ 1 (* 2 (index-column index))))
	       (s-offset (+ (* s-width i) j)))
	  (string-set! s (+ 1 s-offset) #\|)
	  (string-set! s (- s-offset 1) #\|)
	  (string-set! s (+ s-offset s-width) #\-)
	  (string-set! s (- s-offset s-width) #\-)
	  (string-set! s (+ s-offset 1 s-width) #\+)
	  (string-set! s (+ s-offset -1 s-width) #\+)
	  (string-set! s (+ s-offset 1 (- s-width)) #\+)
	  (string-set! s (+ s-offset -1 (- s-width)) #\+)
	  ))
      (grid-indices grid))

    (for-each
      (lambda (index)
	(let* ((color (grid-cell-color grid index))
	       (i (+ 1 (* 2 (index-row index))))
	       (j (+ 1 (* 2 (index-column index))))
	       (s-offset (+ (* s-width i) j))
	       (c-char (if color
			 (integer->char (+ color (char->integer #\0)))
			 #\space))
	       (cell-value (grid-ref grid index))
	       (direction (if (not (index? cell-value))
			    #f
			    (cons
			      (- (index-row cell-value) (index-row index))
			      (- (index-column cell-value) (index-column index))))))
	  (string-set! s s-offset c-char)
	  (if direction
	    (string-set! s (+ s-offset (index-column direction) (* (index-row direction) s-width)) #\space))
	  ))
      (grid-indices grid))

    s))

(define *test-grid* (parse-grid
		      "*123*"
		      "***4*"
		      "**4**"
		      "1**5*"
		      "2*53*"))

(display (grid->string (grid-solve *test-grid*)))
(newline)

(define *hard-grid* (parse-grid
		      "*********"
		      "*******1*"
		      "**2****2*"
		      "**3*4****"
		      "**5*36***"
		      "****78*4*"
		      "****5****"
		      "*1****86*"
		      "***7*****"))
(display (grid->string (grid-solve *hard-grid*)))
(newline)


