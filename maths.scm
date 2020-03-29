;; ======================================================
;;; Basic and common mathmatics procedures
;; ======================================================

(define (square x) (* x x))

;; Takes the average of two numbers
(define (average a b) (/ ( + a b) 2.0))

;; Takes b to the n
(define (expt b n)
  (define (iter a b n)
    (cond ((= n 0) a)
	  ((even? n) (iter a (square b) (/ n 2)))
	  (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

;; Square root
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (next guess x)
    (average guess (/ x guess)))
  (define (iter guess x)
    (if (good-enough? guess)
	guess
	(iter (next guess x) x)))
  (iter 1.0 x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (even? x) (divides? 2 x))

;; Finds the greatest commod divisor of any two numbers
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

;; Succesive fermat-tests determine the primality of a number
(define (flt-prime? n times)
  (define (fermat-test)
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
  (cond ((= times 0) true)
	((fermat-test) (flt-prime? n (- times 1)))
	(else false)))

;; Using the Miller-Rabin (MR) test
(define (mr-expmod base exp m)
  (define (sqmod x) ; Performs test when squaring the modulo
    (define (check mdsqr)
      (if (and (= mdsqr 1)
	       (not (= x 1))
	       (not (= x (- m 1))))
	  0  ; True condition indicates that the number cannot be prime
	  mdsqr))
    (check (remainder (square x) m)))
  (cond ((= exp 0) 1)
	((even? exp)  ; The squaring step, here MR test is done
	 (sqmod (mr-expmod base (/ exp 2) m)))
	(else
	 (remainder (* base (mr-expmod base (- exp 1) m))
		    m))))

;; MR prime
(define (mr-prime? n times)
  (define (mr-test)  ; Passing this test increases confidence that n is prime
    (define (try-it a)
      (= (mr-expmod a (- n 1) n) 1))
    (try-it (+ 1 (random (- n 1)))))
  (define (iter t)
    (cond ((= t 0) true)
	  ((mr-test) (iter (- t 1)))
	  (else false)))
  (if (or (= n 0) (= n 1))
      true
      (iter times)))
  
(define (prime? n) (mr-prime? n 10)) ; Which prime is used?

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



