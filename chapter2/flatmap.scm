(load "common.scm")
(load "chapter2/sequences.scm")

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
	   (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(define (unique-triples n)
  (flatmap
    (lambda (i)
      (flatmap (lambda (j)
	     (map (lambda (k) (list i j k))
		  (enumerate-interval 1 (- j 1))))
	   (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (check-sum? pair s)
  (= s (fold-left + 0 pair)))

(define (make-pair-sum pair)
  (append pair (list (fold-left + 0 pair))))

(define (check-sum-pairs n s)
  (map make-pair-sum
	 (filter
	   (lambda (pair) (= s (fold-left + 0 pair)))
	   (unique-triples n))))

(define (permutations s)
  (if (null? s)
    (list nil)
    (flatmap (lambda (x)
	       (map (lambda (p) (cons x p))
		    (permutations (remove x s))))
	     s)))
