;; Points and segments
(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment) (car segment))

(define (end-segment segment) (cdr segment))

(define (midpoint-segment segment)
  (make-point (average (x-point (start-segment segment))
		       (x-point (end-segment segment)))
	      (average (y-point (start-segment segment))
		       (y-point (end-segment segment)))))

(define (print-segment segment)
  (print-point (start-segment segment))
  (display ":")
  (print-point (end-segment segment)))

(define (make-point x y)
  (cons x y))

(define (x-point point) (car point))

(define (y-point point) (cdr point))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; Rectangles

(define (make-rectangle p1 p2)
  (let ((x1 (x-point p1))
	(x2 (x-point p2))
	(y1 (y-point p1))
	(y2 (y-point p2)))
    (cond ((and (> x1 x2) (> y1 y2)) (cons p2 p1))
	  ((> x1 x2) (cons (make-point x2 y1) (make-point x1 y2)))
	  ((> y1 y2) (cons (make-point x1 y2) (make-point x2 y1)))
	  (else (cons p1 p2)))))

(define (start-rectangle rectangle) (car rectangle))

(define (end-rectangle rectangle) (cdr rectangle))

(define (perimeter-rectangle rectangle)
  (+ (* 2 (- (x-point (end-rectangle rectangle))
	     (x-point (start-rectangle rectangle))))
     (* 2 (- (y-point (end-rectangle rectangle))
	     (y-point (start-rectangle rectangle))))))

(define (area-rectangle rectangle)
  (* (- (x-point (end-rectangle rectangle))
	(x-point (start-rectangle rectangle)))
     (- (y-point (end-rectangle rectangle))
	(y-point (start-rectangle rectangle)))))

(define (print-rectangle rectangle)
  (let ((s1 (make-segment (make-point (x-point (start-rectangle rectangle))
				      (y-point (end-rectangle rectangle)))
			  (end-rectangle rectangle)))
	(s2 (make-segment (start-rectangle rectangle)
			  (make-point (x-point (end-rectangle rectangle))
				      (y-point (start-rectangle rectangle))))))
    (print-segment s1)
    (newline)
    (print-segment s2)))

