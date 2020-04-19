;; Interval arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y)) (- (lower-bound y)))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (multit-interval x y)
  (let ((x1 (lower-bound x))
	(x2 (upper-bound x))
	(y1 (lower-bound y))
	(y2 (upper-bound y)))
    (let ((bounds (cond ((and (positive? x1) (positive? y1)) (cons (* x1 y1) (* x2 y2)))
			((and (positive? x1) (positive? y2)) (cons (* x2 y1) (* x2 y2)))
			((and (positive? x2) (positive? y1)) (cons (* x1 y2) (* x2 y2)))
			((and (positive? x2) (positive? y2)) (cons (max (* x1 y2) (* x2 y1)) (* x2 y2)))
			((positive? x1) (cons (* x2 y1) (* x1 y2)))
			((positive? y1) (cons (* x1 y2) (* x2 y1)))
			((positive? x1) (cons (* x2 y1) (* x1 y2)))
			((positive? y1) (cons (* x1 y2) (* x2 y1)))
			(else (cons (* x2 y2) (* x1 y1))))))
    (make-interval (car bounds) (cdr bounds)))))

(define (divit-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

(define (div-interval x y)
  (let ((y1 (lower-bound y))
	(y2 (upper-bound y)))
    (if (or (= y1 0)
	    (= y2 0)
	    (and (positive? y2) (negative? y1)))
      (error "Divisor interval spans 0 -- DIV-INTERVAL" y)
      (mul-interval x
		    (make-interval (/ 1.0 y2)
				   (/ 1.0 y1))))))

(define (make-interval a b) (cons (min a b) (max a b)))

(define (lower-bound interval) (car interval))

(define (upper-bound interval) (cdr interval))

(define (print-interval interval)
  (display "(")
  (display (lower-bound interval))
  (display ", ")
  (display (upper-bound interval))
  (display ")"))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (/ (* p c) 100.)))
    (make-center-width c w)))

(define (percent interval)
  (/ (* 100. (width interval)) (center interval)))

(define r330 (make-center-percent 330 5))

(define r1k (make-center-percent 1000 5))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))
