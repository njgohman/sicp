;;; ======================================================
; Iterative improvement
;
; The general computational strategy of guessing and
; getting closer.
;
; Edit: 04-05-2020
;;; ======================================================

(load "lowmath.scm")

(define tolerance 0.001)

(define (close-enough? x y)
  (< (abs (- x y)) tolerance))

;; Continuous fraction
(define (cont-frac n d k)
  (define (iter i val)
    (if (= i 0)
	val
	(iter (- i 1)
	      (/ (n i) (+ (d i) val)))))
  (iter (- k 1)
	(/ (n k) (d k))))


;; Iterative improvement
(define (iterative-improve good-enough? improve)
  (lambda (first-guess)
    (define (try guess)
      (let ((next (improve guess)))
	(if (good-enough? guess)
	    next
	    (try next))))
    (try first-guess)))

(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (x)
      (< (abs (- x (f x))) 0.001))
    (lambda (x) (f x)))
   first-guess))
			
;; Better fixed point, with configurable transform
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;; Tranform used in Newton's method
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

;; Average damping for fixed point searches
(define (average-damp f)
  (lambda (x) (average x (f x))))

;; Newton's method
(define (newtons-method g guess)
  (fixed-point-of-transform g newton-transform guess))

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

