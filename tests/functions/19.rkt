(define (sum [n : Integer]) : Integer
  (if (eq? n 0)
      0
      (+ n (sum (- n 1)))))
(sum 5)