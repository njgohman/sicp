;; Demonstrating pairs with cons, for rational numbers
(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (let ((sign (if (and (negative? d)) -1 1)))
      (cons (* sign (/ n g)) (/ (abs d) g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

;; Some arithmetic operations on these rational numbers...
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) 
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)) 
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat x y)
   (= (* (numer x) (denom y))
      (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))

(define one-third (make-rat 1 3))

