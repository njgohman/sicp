;;; ======================================================
; General methods
;
; Adds some useful general structures to simplify
; some of the math definitions.
;
; Edit: 03-31-2020
;;; ======================================================

(load "lowmath.scm")  ; Resolve dependencies

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

;; ======================================================
; Some solvers
;; ======================================================
(define tolerance 0.00001)

(define (close-enough? x y)
  (< (abs (- x y)) tolerance))

;; Finding a root of an equation
(define (half-interval-method f a b)  
  (define (search f neg-point pos-point)
    (let ((midpoint (average neg-point pos-point)))
      (if (close-enough? neg-point pos-point)
	  midpoint
	  (let ((test-value (f midpoint)))
	    (cond ((positive? test-value)
		   (search f neg-point midpoint))
		  ((negative? test-value)
		   (search f midpoint pos-point))
		  (else midpoint))))))
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else
	   (error "Values are not of opposite sign" a b)))))

;; Finding fixed points (i.e. x where f(x) = x)
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))
