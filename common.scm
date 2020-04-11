;;; ======================================================
; Common
;
; Useful and general procedures
; 
; Edit: 04-11-2020
;;; ======================================================

;; Predicates
(define (divides? a b)
  (= (remainder b a) 0))

(define (even? x) (divides? 2 x))

(define (positive? x) (> x 0))

(define (negative? x) (< x 0))

;; Basic and common procedures
(define (square x) (* x x))

(define (cube x) (* x x x))

(define (average a b) (/ ( + a b) 2.0))

(define (inc n) (+ n 1))

(define (identity x) x)

(define pi (* 4 (atan 1 1)))

;; Takes b to the n
(define (expt b n)
  (define (iter a b n)
    (cond ((= n 0) a)
	  ((even? n) (iter a (square b) (/ n 2)))
	  (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

;; Base general logarithm
(define (n-log x b)
  (/ (log x) (log b)))

;; Divisors
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Finds the smallest divisor of any number n
(define (smallest-divisor n)
  (define (next test-divisor)
    (if (= test-divisor 2)
	3
	(+ test-divisor 2)))
  (define (iter n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (iter n (next test-divisor)))))
  (iter n 2))

    
;; Using the Miller-Rabin (MR) test
(define (mr-expmod base exp m)
  (define (sqmod x)
    (let ((mdsqr (remainder (square x) m)))
      (if (and (= mdsqr 1)
	       (not (= x 1))
	       (not (= x (- m 1))))
	  0
	  mdsqr)))
  (cond ((= exp 0) 1)
	((even? exp)  ; The squaring step, here MR test is done
	 (sqmod (mr-expmod base (/ exp 2) m)))
	(else
	 (remainder (* base (mr-expmod base (- exp 1) m))
		    m))))

;; MR prime
(define (mr-prime? n times)
  (define (mr-test)
    (let ((a (+ 1 (random (- n 1)))))
      (= (mr-expmod a (- n 1) n) 1)))
  (define (iter t)
    (cond ((= t 0) true)
	  ((mr-test) (iter (- t 1)))
	  (else false)))
  (if (or (= n 0) (= n 1))
      true
      (iter times)))
  
(define (prime? n) (mr-prime? n 10)) ; Prime predicate alias

;; Derivative
(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

;; Iterative improvement
(define tolerance 0.001)

(define (close-enough? x y)
  (< (abs (- x y)) tolerance))

;; Continuous fraction
(define (cont-frac n d k)
  (define (iter i val)
    (if (= i 0)
	(* 1.0 val)
	(iter (- i 1)
	      (/ (n i) (+ (d i) val)))))
  (iter (- k 1)
	(/ (n k) (d k))))

;; Nested expression
(define (nested-acc op r term k)
  (define (iter i val)
    (if (= i 0)
      (* 1.0 val)
      (iter (- i 1) ((op i) (term i) val))))
  (iter k r))

;; Fixed point search
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

;; Square root
(define (sqrt x)
  (newtons-method (lambda (y) (- x (square y))) 1.0))

;; Some procedure modifiers
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (double f)
  (compose f f))

(define (repeated p n)
  (cond ((= n 0) (lambda (x) x))
	((= n 1) p)
	(else (lambda (x) (p ((repeated p (- n 1)) x))))))

