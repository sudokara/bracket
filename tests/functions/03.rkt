(define (inc [x : Integer]) : Integer
(dec x))
(define (dec [x : Integer]) : Integer
(- x 1))
(let ([vecFunc (vector inc dec)])
  ((vector-ref vecFunc 0) 43))
