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

(define (repeated f n)
  (lambda (x)
    (define (iter i composed)
      (if (= i 0)
	  composed
	  (iter (- i 1) (f composed))))
    (iter (- n 1) (f x))))
