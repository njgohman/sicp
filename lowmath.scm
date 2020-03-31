;;; ======================================================
; Low math
;
; Adds some basic math to the scheme primitives.
; 
; Edit: 03-31-2020
;;; ======================================================

;; ======================================================
; Predicates
;; ======================================================

(define (divides? a b)
  (= (remainder b a) 0))

(define (even? x) (divides? 2 x))

(define (positive? x) (> x 0))

(define (negative? x) (< x 0))


;; ======================================================
; Basic and common procedures
;; ======================================================
(define (square x) (* x x))

(define (cube x) (* x x x))

(define (average a b) (/ ( + a b) 2.0))

(define (inc n) (+ n 1))

(define (identity x) x)

;; Takes b to the n
(define (expt b n)
  (define (iter a b n)
    (cond ((= n 0) a)
	  ((even? n) (iter a (square b) (/ n 2)))
	  (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

;; ======================================================
; Divisors
;; ======================================================
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

;; ======================================================
; Primality tests
;; ======================================================

;; Using smallest divisor method
(define (smd-prime? n)
  (= n (smallest-divisor n)))

;; Using Fermat's Little Theorem, the exponential modulo procedure
(define (expmod base exp m)
  (cond ((= exp 0 ) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

;; Succesive fermat-tests determine the primality of a number
(define (flt-prime? n times)
  (define (fermat-test)
    (let ((a (+ 1 (random (- n 1)))))
      (= (expmod a n n) a)))
  (cond ((= times 0) true)
	((fermat-test) (flt-prime? n (- times 1)))
	(else false)))
    
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

;; Searching for prime numbers, prime numbers take longer to test
(define (search-for-primes n count)
  (cond ((= count 0) true)
	((even? n)
	 (search-for-primes (+ n 1) count))
	((prime? n)
	 (write n)
	 (write-char #\space)
	 (search-for-primes (+ n 2) (- count 1)))
	(else
	 (search-for-primes (+ n 2) count))))

;; Timing prime
(define (timed-prime? n)
  (with-timings
   (lambda ()
     (prime? n))
   (lambda (run-time gc-time real-time)
     (write (internal-time/ticks->seconds run-time))
     (write-char #\space)
     (write (internal-time/ticks->seconds gc-time))
     (write-char #\space)
     (write (internal-time/ticks->seconds real-time))
     (newline))))

;;; ============================================================



