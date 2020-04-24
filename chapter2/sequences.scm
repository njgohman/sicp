(load "common.scm")

(define nil '())

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		       (enumerate-tree (cdr tree))))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))

(define (count-leaves t)
  (accumulate +
	      0
	      (map (lambda (x)
		     (if (pair? x)
		       (length x)
		       1))
		   t)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map car seqs))
	  (accumulate-n op init (map cdr seqs)))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
	  sequence))

;; Flatmap
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
