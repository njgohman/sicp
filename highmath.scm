;;; ======================================================
; High math
;
; Builds some more sophisticated math procedures using
; procedures from low math and general methods.
;
; Edit: 03-31-2020
;;; ======================================================

(load "general_methods.scm")  ; Resolve dependencies

;; ======================================================
; Additional structures, general methods
;; ======================================================

;; Series sum
(define (sum term a next b)
  (accumulate + 0.0 term a next b))

;; Series product
(define (product term a next b)
  (accumulate * 1.0 term a next b))

;; Iterative
(define (cont-frac n d k)
  (define (iter i val)
    (if (= i 0)
	val
	(iter (- i 1)
	      (/ (n i) (+ (d i) val)))))
  (iter (- k 1)
	(/ (n k) (d k))))

;; ======================================================
; Higher level mathematics procedures
;; ======================================================

;; Square root
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0))

;; Cube root
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
	       1.0))

(define (factorial n)
  (if (or (= n 1) (= n 0))
      1
      (product identity 1 inc n)))

;; ======================================================
; Estimations of some useful constants
;; ======================================================

(define (pi) (half-interval-method sin 3.1 3.2))

;; Golden ratio
(define (phi)
  (fixed-point
   (average-damp (lambda (x) (+ 1 (/ 1 x))))
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
