(load "common.scm")
(load "sequences.scm")

(define (dot-product v w)
  (fold-right + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (matrix-*-vector cols r)) m)))

