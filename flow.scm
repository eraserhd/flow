
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

(define (grid-width grid)
  (vector-ref grid 0))

(define (grid-height grid)
  (quotient
    (- (vector-length grid) 1)
    (grid-width grid)))

(define make-index cons)
(define index-row car)
(define index-column cdr)

(define (grid-cell-index/internal grid index)
  (+ (* (index-row index)
        (grid-width grid))
     (index-column index)))

(define (grid-ref grid index)
  (vector-ref grid (grid-cell-index/internal grid index)))

(define (grid-set! grid index value)
  (vector-set! grid (grid-cell-index/internal grid index)))

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

(define *test-grid* (parse-grid
		      "*1***2**"
		      "******1*"
		      "2*******"))
(write (grid-height *test-grid*))
(write (grid-indices *test-grid*))

