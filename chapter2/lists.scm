
(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))  

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (last-pair listx)
  (if (null? (cdr listx))
    (car listx)
    (last-pair (cdr listx))))

(define nil '())

(define (reverse listx)
  (define (iter result residual)
    (if (null? residual)
      result
      (iter (cons (car residual) result) (cdr residual))))
  (iter nil listx))

(define (same-parity x . y)
  (define (iter same-parity? residual)
    (cond ((null? residual) nil)
	  ((same-parity? (car residual))
	   (cons (car residual) (iter same-parity? (cdr residual))))
	  (else
	    (iter same-parity? (cdr residual)))))
  (if (even? x)
    (cons x (iter even? y))
    (cons x (iter odd? y))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(define (square-list items)
  (map (lambda (x) (square x))
       items))

