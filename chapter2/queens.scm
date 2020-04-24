(load "common.scm")
(load "chapter2/sequences.scm")


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      empty-board
      (filter
	(lambda (positions) (safe? positions))
	(flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (cons (list k new-row)
	rest-of-queens))

(define empty-board (list nil))

(define (print-solution solution)
  (for-each
    (lambda (sol)
      (display sol)
      (newline))
    solution))

(define (safe? positions)
  (let ((q1 (car positions)))
    (fold-right
      (lambda (q cumulative-safe)
	(and cumulative-safe
	     (not (= (cadr q1) (cadr q)))
	     (not (= (abs (- (car q1) (car q)))
		     (abs (- (cadr q1) (cadr q)))))))
      #t
      (cdr positions))))
