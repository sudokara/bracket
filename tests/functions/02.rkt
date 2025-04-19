(define (inc [x : Integer]) : Integer
(+ x 1))
(define (dec [x : Integer]) : Integer
(- x 1))
(let ([vecFunc (vector inc dec)])
  ((vector-ref vecFunc 0) 41))
