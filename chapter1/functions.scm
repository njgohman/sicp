;;; ======================================================
; Functions
;
; Some general, differentiable mathmatical functions.
;
; Edit: 04-05-2020
;;; ======================================================

(load "highmath.scm")

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

(define (quadratic a b)
  (lambda (x)
    (+ (square x) (* a x) b)))

(define (step a b n)
  (/ (- b a) n))

(define (range a b n)
  (lambda (i)
    (+ a (* i (step a b n)))))

(define (function-report f a b n)
  (define (iter i)
    (let ((x ((range a b n) i)))
      (write x)
      (display ",")
      (write (f x))
      (newline)
      (if (= i n)
	  true
	  (iter (+ i 1)))))
  (iter 0))

(define (smooth f)
  (let ((dx 0.0001))
    (lambda (x)
      (/ (+ (f (- x dx))
	    (f x)
	    (f (+ x dx)))
	 3.0))))

(define (nsmooth f n)
  (repeated (smooth f) n))

