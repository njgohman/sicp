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
