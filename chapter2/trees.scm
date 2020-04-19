(load "chapter2/lists.scm")

(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(define (deep-reverse listx)
  (define (iter result residual)
    (cond ((null? residual)
	   result)
	  ((not (pair? residual))
	   residual)
	  (else
	    (iter
	      (cons (iter nil (car residual)) result)
	      (cdr residual)))))
  (iter nil listx))

(define (fringe xtree)
  (cond ((null? xtree) nil)
	((not (pair? xtree)) (list xtree))
	(else
	  (append (fringe (car xtree))
		  (fringe (cdr xtree))))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	   (scale-tree sub-tree factor)
	   (* sub-tree factor)))
       tree))

(define (tree-map func tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	   (tree-map func sub-tree)
	   (func sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))
