(load "common.scm")

(define (root-test x)
  (flatmap
    (lambda (rest-of-queens)
      (map (lambda (new-row)
	     (adjoin-position new-row 1 rest-of-queens))
	   (enumerate-interval 1 3)))
    x))

(define (fold-test positions)
  (let ((row (cadar positions)))
    (fold-right
      (lambda (a b)
	(and b (not (= row (cadr a)))))
      #t
      (cdr positions))))
