;;; ======================================================
; Modifiers
;
; General modifications to procedures
;
; Edit: 04-05-2020
;;; ======================================================

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (double f)
  (compose f f))

(define (repeated p n)
  (cond ((= n 0) (lambda (x) x))
	((= n 1) p)
	(else (lambda (x) (p ((repeated p (- n 1)) x))))))

(define (build n d b)
  (/ n (+ d b)))

(define (repeated-build k n d b)
  (* 1.0 ((repeated (lambda (x) (build n d x)) k) b)))


