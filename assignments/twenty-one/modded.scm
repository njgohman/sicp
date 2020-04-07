;;; Additions to base, to complete the assignment

(load "base.scm")

;; The simple stop strategy, problem 2
(define (stop-at n)
  (lambda (hand up-card)
    (if (< (hand-total hand) n)
	true
	false)))

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

(define (p4test player-strategy house-strategy)
  (test-strategy (watch-player (stop-at 16))
		 (watch-player (stop-at 15))
		 2))
	 
    
