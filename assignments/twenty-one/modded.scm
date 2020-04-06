;;; Additions to base, to complete the assignment

(load "base.scm")

;; The simple stop strategy
(define (stop-at n)
  (lambda (hand up-card)
    (if (< (hand-total hand) n)
	true
	false)))
