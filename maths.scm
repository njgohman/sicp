;; ======================================================
;;; Basic and common mathmatics procedures
;; ======================================================

(define (square x) (* x x))

(define (even? x) (= (remainder x 2) 0))

(define (divides? a b)
  (= (remainder b a) 0))

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
;;; Testing for primality
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

;; Checks the expmod condition for a single random number

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; Succesive fermat-tests determine the primality of a number

(define (flt-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (flt-prime? n (- times 1)))
	(else false)))

(define (prime? n) (flt-prime? n 100)) ; Which prime is used?

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



