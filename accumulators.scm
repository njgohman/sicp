;; ======================================================
;;; Accumulation structures
;; ======================================================

(define (inc n) (+ n 1))
(define (identity x) x)

;; Accumulation
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

(define (sum term a next b)
  (accumulate + 0.0 term a next b))

(define (product term a next b)
  (accumulate * 1.0 term a next b))

(define (factorial n)
  (if (or (= n 1) (= n 0))
      1
      (product identity 1 inc n)))

;; Sum of cubes
(define (sum-cubes a b)
  (sum cube a inc b))

;; Sum of integers
(define (sum-ints a b)
  (sum identity a inc b))

;; ======================================================
;;; Integrations
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


