;;; ======================================================
; High math
;
; Builds some more sophisticated math procedures using
; procedures from low math and general methods.
;
; Edit: 04-05-2020
;;; ======================================================

(load "iterative_improve.scm")  ; Resolve dependencies
(load "modifiers.scm")

;; ======================================================
; Accumulation structures
;; ======================================================
(define (filtered-accumulate predicate? combiner null-value term a next b)
  (define (iter x result)
    (cond ((> x b) result)
	  ((predicate? x)
	   (iter (next x) (combiner result (term x))))
	  (else
	   (iter (next x) result))))
  (iter a null-value))

(define (accumulate combiner null-value term a next b)
  (filtered-accumulate (lambda (x) true) combiner null-value term a next b))

;; Series sum
(define (sum term a next b)
  (accumulate + 0.0 term a next b))

;; Series product
(define (product term a next b)
  (accumulate * 1.0 term a next b))

;; ======================================================
; Higher level mathematics procedures
;; ======================================================

(define (sqrt x)
  (newtons-method (lambda (y) (- x (square y) 1.0))))

;; Cube root
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
	       1.0))

(define (n-root x n)
  (fixed-point ((repeated average-damp (floor (nlog n 2)))
		(lambda (y) (/ x (expt y (- n 1)))))
	       1.0))

(define (factorial n)
  (if (or (= n 1) (= n 0))
      1
      (product identity 1 inc n)))

;; ======================================================
; Estimations of some useful constants
;; ======================================================

(define (pi)
  (newtons-method (lambda (x) (sin x)) 3.0))

#;(define (pi)
  (fixed-point-of-transform (lambda (x) (sin x))
			    newton-transform
			    3.0))

(define (phi)
  (fixed-point-of-transform (lambda (x) (+ 1 (/ 1 x)))
			    average-damp
			    1.0))

;; Base of the natural logarithm
(define (e)
  (+ 2
     (cont-frac
      (lambda (i) 1.0)
      (lambda (i)
	(cond ((= i 1) 1)
	      ((= (remainder (+ i 1) 3) 0)
	       (* (/ (+ i 1) 3) 2))
	      (else 1)))
      10)))

;; Estimated tangent function
(define (tan-cf x k)
  (cont-frac
   (lambda (i)
     (if (= i 1) x
	 (* -1 (square x))))
   (lambda (i)
     (if (= i 1) 1
	 (- (* i 2) 1)))
   k))

;; ======================================================
; Integrations
;; ======================================================

;; Using the definition of the integral
(define (definite-integral f a b dx)
  (* (sum f
	  (+ a (/ dx 2))
	  (lambda (x) (+ x dx))
	  b)
     dx))

;; Using Simpson's Rule
(define (simpson-integral f a b n)
  (define (simpson-term x)
    (let ((k (/ (* n (- x a)) (- b a)))
	  (term (f x)))
      (cond ((or (= k 0)
		 (= ( - n k) 0))
	     term)
	    ((or (even? k)
		 (= (- n k) 2))
	     (* 2 term))
	    (else (* 4 term)))))
  (let ((h (/ (- b a) n)))
    (* (/ h 3.0)
       (sum simpson-term
	    a
	    (lambda (x) (+ x h))
	    b))))

;; Integral alias
(define (integral f a b n) (simpson-integral f a b n))
