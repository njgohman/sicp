(load "common.scm")
(load "chapter2/lists.scm")

(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (let ((b (cdr branch)))
    (if (null? (cdr b))
      (car b)
      b)))

(define (total-weight mobile)
  (if (not (pair? mobile))
    mobile
    (+ (total-weight (branch-structure (left-branch mobile)))
       (total-weight (branch-structure (right-branch mobile))))))

(define (balanced? mobile)
  (if (not (pair? mobile))
    true
    (and (= (* (branch-length (left-branch mobile))
	       (total-weight (branch-structure (left-branch mobile))))
	    (* (branch-length (right-branch mobile))
	       (total-weight (branch-structure (right-branch mobile)))))
	 (balanced? (branch-structure (left-branch mobile)))
	 (balanced? (branch-structure (right-branch mobile))))))
