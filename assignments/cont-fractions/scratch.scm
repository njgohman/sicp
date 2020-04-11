(define (phi-op x k)
  (nested-acc
    (lambda (i)
      (if (even? i)
	(lambda (a b) (+ a b))
	(lambda (a b) (* a (sqrt b)))))
    0
    (lambda (i)
      (if (even? i) x 1))
    k))

(define (sin-nest x k)
  (nested-acc
    (lambda (i)
      (if (even? (+ (- k i) 1))
	(lambda (a b) (- b a))
	(lambda (a b) (+ b a))))
    0
    (lambda (i)
      (let ((a (+ (* 2 (- k i)) 1)))
	(/ (expt x a)
	   (factorial a))))
    k))

(define (build n d b)
  (/ n (+ d b)))

(define (repeated-build k n d b)
  (* 1.0 ((repeated (lambda (x) (build n d x)) k) b)))

(define phi
  (/ 1 (repeated-build 20 1 1 1)))

(define (rational k x)
  (repeated-build (- k 1) 1 1 (+ 1 x)))
