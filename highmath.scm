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
; Higher level mathematics procedures
;; ======================================================

;; Square root
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
	       1.0))

;; Series sum
(define (sum term a next b)
  (accumulate + 0.0 term a next b))

;; Series product
(define (product term a next b)
  (accumulate * 1.0 term a next b))

(define (factorial n)
  (if (or (= n 1) (= n 0))
      1
      (product identity 1 inc n)))

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
