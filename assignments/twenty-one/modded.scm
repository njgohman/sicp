;;; Additions to base, to complete the assignment

(load "base.scm")

;; Testing multiple strategies, problem 3
(define (test-strategy player-strategy house-strategy n)
  (define (iter wins i)
    (if (= i 0)
	wins
	(iter (+ wins (twenty-one player-strategy house-strategy)) (- i 1))))
  (iter 0 n))

;; Watching strategy, problem 4
(define (watch-player strategy)
  (lambda (hand up-card)
    (display "Opponent up card: ")
    (write up-card)
    (display " Hand total: ")
    (write (hand-total hand))
    (display " Decision: ")
    (let ((decision (strategy hand up-card)))
      (if decision
	  (display "Hit")
	  (display "Stand"))
      (newline)
      decision)))

;; The simple stop strategy, problem 2
(define (stop-at n)
  (lambda (hand up-card)
    (if (< (hand-total hand) n)
	true
	false)))

;; The Louis strategy, problem 5
(define (louis hand up-card)
  (let ((total (hand-total hand)))
    (cond ((< total 12) true)
	  ((> total 16) false)
	  ((and (= total 12)
		(< up-card 4))
	   true)
	  ((and (= total 16)
		(= up-card 10))
	   false)
	  ((> up-card 6) true)
	  (else false))))

;; The both strategy of problem 6
(define (both strategy-one strategy-two)
  (lambda (hand up-card)
    (if (and (strategy-one hand up-card)
	     (strategy-two hand up-card))
	true
	false)))

;;; Tests

(define (p4test player-strategy house-strategy)
  (test-strategy (watch-player (stop-at 16))
		 (watch-player (stop-at 15))
		 2))


